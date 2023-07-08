{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module App (  api
            , server
            , theApplication
            , theApplicationWithSettings
            , testApplication
            , TheAPI
            ) where

import Configuration
    ( defaultConfiguration,
      updateHostname,
      updateGithubAccessToken,
      updateGithubRoot,
      Configuration(getHostname) )

import Servant
  
import           Control.Concurrent          (forkIO, killThread)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)

import           Control.Exception           (bracket)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, asks, runReaderT)
import           Control.Monad.Trans.State  (StateT, runStateT)

import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE
import Crypto.JWT (signClaims, SignedJWT)
import Crypto.JOSE.JWS (newJWSHeader)

import System.Environment (lookupEnv)

import Servant.Server
import Servant.Auth.Server as SAS

import Configuration.Dotenv (loadFile, defaultConfig)

import Data.Aeson
import Data.Text ( Text )  
import qualified Database.Redis as R
import Data.String.Conversions (cs)
import qualified Data.ByteString as BS
import Network.Wai.Handler.Warp
    (setLogger, setPort, getPort, runSettings, defaultSettings, Port(..), Settings(..) )

import Data.X509.File ( readKeyFile )
import Data.X509 ( PrivKey(PrivKeyRSA) )
import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as C8
import Network.Wai (Middleware, remoteHost)

import Data.Pool (createPool, Pool, withResource)
import Control.Monad (void)

import JsonWorkProof
import Authentication (AuthenticatedUser(..), authCheck)
import AppM ( AppCtx(..), AppM(..), MonadDB(..), HasConfiguration(..), HasSymmetricJWK(..) )
import MockM ( MockM(..) )

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except

import qualified LTI

import qualified User
import qualified Course
import qualified Authentication
import qualified Learner

type TheAPI = Authentication.API :<|>
  -- User.API :<|> Course.API :<|>
  Learner.API

api :: Proxy TheAPI
api = Proxy

server :: (MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasSymmetricJWK r, MonadError ServerError m) => SAS.CookieSettings -> SAS.JWTSettings -> ServerT TheAPI m
server _cookies jwt =
  Authentication.server _cookies jwt :<|>
--  User.server _cookies jwt :<|>
--  Course.server _cookies jwt :<|>
  Learner.server _cookies jwt

--runRedis :: R.Redis a -> AppM a
--runRedis redis = do
--   conn <- asks getConnection
--   liftIO $ R.runRedis conn redis

--  (userToken jwt u) :<|> (userAuthorize u)
 
-- where getBooks :: AppM [Book]
--       getBooks = do
--         hello <- runRedis $ do
--           R.get "hello"
--         case hello of
--           Right result -> return [Book $ show result] 
--           Left _ -> throwError err500
--       getSecure :: AuthResult AuthenticatedUser -> AppM [Book]
--       getSecure (SAS.Authenticated _adminAuthenticatedUser)  = pure [] 
--       getSecure _ = throwError err401 
  
-- natural transformation from the AppM monad to the Handler
nt :: AppCtx -> AppM a -> Handler a
nt s x = runReaderT (runApp x) s

appWithContext :: AppCtx -> IO Application
appWithContext ctx = do
  let myKey = getJWK ctx 
      jwtCfg = defaultJWTSettings myKey
      pool = getPool ctx
  withResource pool $ \conn -> do
    let authCfg :: BasicAuthCheck AuthenticatedUser = authCheck (_getConfiguration ctx) conn 
    let cookies = defaultCookieSettings 
    let cfg = authCfg :. conn :. jwtCfg :. cookies :. EmptyContext
    pure $ serveWithContext api cfg $ hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCheck AuthenticatedUser, R.Connection,  SAS.CookieSettings, SAS.JWTSettings]) (nt ctx) (server cookies jwtCfg)

middleware :: Middleware
middleware = id

testApplication :: IO Application
testApplication = do
  let hostname = Just "localhost"
  symmetricJwk <- generateKey
  let config = updateHostname hostname defaultConfiguration
  let port = 8000
  let ctx = AppCtx { getJWK = symmetricJwk,
                     _getSymmetricJWK = symmetricJwk,
                     _getConfiguration = config,
                     getPool = undefined }
  let myKey = getJWK ctx 
      jwtCfg = defaultJWTSettings myKey
  let authCfg :: BasicAuthData -> IO (BasicAuthResult AuthenticatedUser)
      authCfg (BasicAuthData login _ ) = do
           pure $ Servant.Authorized AuthenticatedUser { username = cs login
                                                       , domain = "localhost"
                                                       , subject = Nothing
                                                       , audience = Nothing
                                                       }
  let cookies = defaultCookieSettings
  let cfg = (BasicAuthCheck authCfg) :. jwtCfg :. cookies :. EmptyContext
  pure $ serveWithContext api cfg $ hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCheck AuthenticatedUser, R.Connection,  SAS.CookieSettings, SAS.JWTSettings]) (nt' ctx) (server cookies jwtCfg)
    where nt' :: AppCtx -> MockM a -> Handler a
          nt' s x = fst <$> runStateT (runReaderT (runMock x) s) Map.empty

theApplicationWithSettings :: Settings -> IO Application
theApplicationWithSettings settings = do
  -- with this, lookupEnv will fetch from .env or from an environment variable
  _ <- Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  hostname <- lookupEnv "HOSTNAME"
  
  root <- lookupEnv "GITHUB_ROOT"
  accessToken <- lookupEnv "GITHUB_ACCESS_TOKEN"
  let config = updateGithubRoot root $ updateGithubAccessToken accessToken $ updateHostname hostname $ defaultConfiguration
  
  privateKeyFilename <- lookupEnv "PRIVATE_KEY"
  certs <- maybe (pure []) readKeyFile privateKeyFilename
  jwkRsa <- case certs of
    [PrivKeyRSA key] -> pure $ fromRSA key
    _ -> error "Expected the file $PRIVATE_KEY to contain an RSA key"

  symmetricJwk <- generateKey

  putStrLn $ "Listening on port " ++ show (getPort settings)

  redisConnectionSocket <- lookupEnv "REDIS_SOCKET"
  let f s = Right R.defaultConnectInfo { R.connectPort = R.UnixSocket s }
  let connectSocket = maybe (Right R.defaultConnectInfo) f redisConnectionSocket

  redisConnectionString <- lookupEnv "REDIS"
  let connectInfo = maybe connectSocket R.parseConnectInfo redisConnectionString

  let connectInfo' = case connectInfo of
                       Left e -> error e
                       Right c -> c

  pool <- createPool (R.checkedConnect connectInfo') -- creating connection
          (\conn -> void $ R.runRedis conn R.quit) -- clean-up action
          1 -- number of sub-pools
          60 -- how long in seconds to keep unused connections open
          50 -- maximum number of connections
  
  conn <- either error R.checkedConnect connectInfo

  let context = AppCtx { getJWK = jwkRsa, _getSymmetricJWK = symmetricJwk, _getConfiguration = config, getPool = pool }

  appWithContext context

theApplication :: IO Application
theApplication = do
  port <- lookupEnv "PORT"
  let settings = maybe id (setPort . read) port defaultSettings
  theApplicationWithSettings settings
