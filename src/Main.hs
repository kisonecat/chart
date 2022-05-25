{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import Configuration

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
  
import           Control.Concurrent          (forkIO, killThread)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)

import           Control.Exception           (bracket)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, asks, runReaderT)

import           GHC.Generics                (Generic)
import           Network.Wai.Handler.Warp    (run)

import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE 
import Crypto.JWT (signClaims, SignedJWT)
import Crypto.JOSE.JWS (newJWSHeader)

import Network.HTTP.Media ((//), (/:))

import System.Environment (lookupEnv)

import Servant.Server
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp
    (setLogger, setPort, getPort, runSettings, defaultSettings )
import Configuration.Dotenv (loadFile, defaultConfig)

import           Data.Aeson                  (FromJSON, ToJSON)
import Data.Text ( Text )  
import qualified Database.Redis as R
import Data.String.Conversions (cs)
import qualified Data.ByteString as BS

data AuthenticatedUser = AuthenticatedUser { name :: !Text
                                           , chart :: !Text
                                           }
                       deriving (Show, Generic)

data User = User { userId :: !Text 
                 , firstName :: !Text
                 , lastName :: !Text
                 , email :: !Text
                 }
          deriving (Show, Generic)

instance FromJSON AuthenticatedUser
instance ToJSON AuthenticatedUser 
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

data JSONWebToken  
  
instance Accept JSONWebToken where
   contentType _ = "application" // "jwt" 

instance {-# OVERLAPPING #-} ToCompact a => MimeRender JSONWebToken a where
    mimeRender _ = encodeCompact

-- this is just a fake stub for now
authCheck :: Configuration -> R.Connection
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck config _connection (BasicAuthData login _password) = pure $
  maybe SAS.Indefinite Authenticated $ Just $ AuthenticatedUser { name = cs login, chart = cs $ getHostname config }

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type UserCrudAPI = Get '[JSON] User :<|> (ReqBody '[JSON] User :> (Put '[JSON] User :<|> Patch '[JSON] User))
type UserTokenAPI = SAS.Auth '[SA.BasicAuth] AuthenticatedUser :> ( ("token" :> Get '[JSONWebToken] SignedJWT ) :<|> ("authorize" :> Get '[JSON] ()) )
type UserAPI = SAS.Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser :> "users" :> Capture "username" Text :> (UserTokenAPI :<|> UserCrudAPI)

newtype Book = Book String deriving (Show, Generic)
instance ToJSON Book
instance FromJSON Book

type GetBooks = Get '[JSON] [Book]

type SecureBooksAPI =
  SAS.Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser :> Get '[JSON] [Book]

type BooksAPI = ("books" :> GetBooks) :<|> ("secure" :> SecureBooksAPI )

api :: Proxy UserTokenAPI 
api = Proxy

data AppCtx = AppCtx { getConfiguration :: Configuration
                     , getConnection :: R.Connection 
                     }
                       
type AppM = ReaderT AppCtx Handler

runRedis :: R.Redis a -> AppM a
runRedis redis = do
   conn <- asks getConnection
   liftIO $ R.runRedis conn redis



bestAlg :: AuthenticatedUser -> SAS.JWTSettings -> IO (Either Crypto.JOSE.Error Crypto.JWT.SignedJWT)
bestAlg u jwt  = runExceptT $ do
  let jwk = signingKey jwt
  alg <- JOSE.bestJWSAlg jwk
  let claims = encodeJWT u
  signClaims jwk (newJWSHeader ((), alg)) claims

userToken :: SAS.JWTSettings -> AuthResult AuthenticatedUser -> AppM SignedJWT 
userToken jwt (SAS.Authenticated u) = do
  --let jwk = signingKey jwt
  --alg <- liftIO $ bestAlg jwt 
  --let claims = encodeJWT u
  --signed <- signClaims jwk (newJWSHeader ((), alg)) claims
  signed <- liftIO $ bestAlg u jwt
  case signed of
     Left err -> throwError err500
     -- Right j -> pure $ Book $ cs $ encodeCompact j
     Right j -> pure j
  
userToken _ _ = throwError err401
  
userAuthorize :: AuthResult AuthenticatedUser -> AppM () 
userAuthorize (SAS.Authenticated _adminAuthenticatedUser) = pure () 
userAuthorize _ = throwError err401

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT UserTokenAPI AppM
server _cookies jwt u = (userToken jwt u) :<|> (userAuthorize u)
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

app :: AppCtx -> IO Application
app ctx = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck (getConfiguration ctx) (getConnection ctx)
      cookies = defaultCookieSettings 
      cfg = jwtCfg :. cookies :. authCfg :. EmptyContext
  pure $ serveWithContext api cfg $ hoistServerWithContext api (Proxy :: Proxy '[BasicAuthData -> IO (AuthResult AuthenticatedUser), SAS.CookieSettings, SAS.JWTSettings]) (nt ctx) (server cookies jwtCfg)

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

  putStrLn $ "Listening on port " ++ show (getPort settings)

  redisConnectionString <- lookupEnv "REDIS"
  let connectInfo = maybe (Right R.defaultConnectInfo) R.parseConnectInfo redisConnectionString
  conn <- either error R.checkedConnect connectInfo

  let context = AppCtx { getConfiguration = config, getConnection = conn }
      
  runSettings settings =<< app context
