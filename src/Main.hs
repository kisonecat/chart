{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

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

import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE
import Crypto.JWT (signClaims, SignedJWT)
import Crypto.JOSE.JWS (newJWSHeader)

import System.Environment (lookupEnv)

import Servant.Server
import Servant.Auth.Server as SAS

import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp
    (setLogger, setPort, getPort, runSettings, defaultSettings )
import Configuration.Dotenv (loadFile, defaultConfig)

import Data.Aeson
import Data.Text ( Text )  
import qualified Database.Redis as R
import Data.String.Conversions (cs)
import qualified Data.ByteString as BS
  
import Data.X509.File ( readKeyFile )
import Data.X509 ( PrivKey(PrivKeyRSA) )

import qualified Data.ByteString.Char8 as C8
import Network.Wai (Middleware, remoteHost)
import Network.Wai.Logger (withStdoutLogger)

import Data.Pool (createPool, Pool, withResource)
import Control.Monad (void)

import JsonWorkProof
import Authentication (AuthenticatedUser, authCheck)
import AppM ( AppCtx(..), AppM )

import qualified LTI

import qualified User
import qualified Course
import qualified Authentication
import qualified Learner

type TheAPI = Authentication.API :<|> User.API :<|> Course.API :<|> Learner.API

api :: Proxy TheAPI
api = Proxy

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT TheAPI AppM
server _cookies jwt =
  Authentication.server _cookies jwt :<|>
  User.server _cookies jwt :<|>
  Course.server _cookies jwt :<|>
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
nt s x = runReaderT x s

appWithContext :: AppCtx -> IO Application
appWithContext ctx = do
  let myKey = getJWK ctx 
      jwtCfg = defaultJWTSettings myKey
      pool = getPool ctx
  withResource pool $ \conn -> do
    let authCfg = authCheck (getConfiguration ctx) conn 
    let cookies = defaultCookieSettings 
    let cfg = conn :. jwtCfg :. cookies :. authCfg :. EmptyContext
    pure $ serveWithContext api cfg $ hoistServerWithContext api (Proxy :: Proxy '[R.Connection, BasicAuthData -> IO (AuthResult AuthenticatedUser), SAS.CookieSettings, SAS.JWTSettings]) (nt ctx) (server cookies jwtCfg)

--middleware :: R.Connection -> Middleware
--middleware conn = rateLimiting strategy
--    where backend = redisBackend conn
--          getKey = pure . C8.pack . show . remoteHost
--          strategy = fixedWindow backend 59 1 getKey

middleware :: R.Connection -> Middleware
middleware _ = id

-- main2 :: IO ()
-- main2 = do
--   let jwp = decodeJWP "eyJ0eXAiOiAiSldQIiwgImFsZyI6ICJTSEEyNTYiLCAiZGlmIjogMTB9.eyJoZWxsbyI6ICJ3b3JsZCIsICJjb3VudCI6IDg4LCAiZXhwIjogMTY1Mzc3ODMzNC4wNDY0Mzl9.y1Iw4WeHBIuUT_ncwpdqDQBW4"

--   r <- either error JsonWorkProof.verify jwp 
--   putStrLn $ show $ r

main :: IO ()
main = do
  -- with this, lookupEnv will fetch from .env or from an environment variable
  _ <- Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  hostname <- lookupEnv "HOSTNAME"

  port <- lookupEnv "PORT"
  let settings = maybe id (setPort . read) port defaultSettings
  
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

  let context = AppCtx { getJWK = jwkRsa, getSymmetricJWK = symmetricJwk, getConfiguration = config, getPool = pool }

  withStdoutLogger $ \aplogger -> do
    let settingsWithLog = setLogger aplogger settings
    theApp <- appWithContext context
    runSettings settingsWithLog $ middleware conn theApp
