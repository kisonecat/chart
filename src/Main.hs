{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where


import Configuration

import Servant
    ( serveWithContext,
      Context(EmptyContext),
      Context( (:.) ),
      type (:>),
      Get,
      HasServer(ServerT),
      Proxy(..),
      Application,
      Handler,
      hoistServer,
      JSON ) 

import Servant.Auth as SA
import Servant.Auth.Server as SAS
  
import           Control.Concurrent          (forkIO, killThread)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)

import           Control.Exception           (bracket)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Except        (liftEither, throwError)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, asks, runReaderT)

import           GHC.Generics                (Generic)
import           Network.Wai.Handler.Warp    (run)

import System.Environment (lookupEnv)

import Servant.Server (Handler, Server, Application, serve, err500)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp
    (setLogger, setPort, getPort, runSettings, defaultSettings )
import Configuration.Dotenv (loadFile, defaultConfig)

import           Data.Aeson                  (FromJSON, ToJSON)
import Data.Text ( Text )  
import qualified Database.Redis as R
import Data.String.Conversions (cs)

data User = User { username :: !Text
                 , chart :: !Text
                 }
          deriving (Show, Generic)

instance FromJSON User
instance ToJSON User 
instance ToJWT User
instance FromJWT User

-- this is just a fake stub for now
authCheck :: Configuration -> R.Connection
          -> BasicAuthData
          -> IO (AuthResult User)
authCheck config _connection (BasicAuthData login _password) = pure $
  maybe SAS.Indefinite Authenticated $ Just $ User { username = cs login, chart = cs $ getHostname config }

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

newtype Book = Book String deriving (Show, Generic)
instance ToJSON Book
instance FromJSON Book

type GetBooks = Get '[JSON] [Book]
type BooksAPI = "books" :> (GetBooks)

api :: Proxy BooksAPI
api = Proxy

data AppCtx = AppCtx { getConfiguration :: Configuration
                     , getConnection :: R.Connection 
                     }
                       
type AppM = ReaderT AppCtx Handler

runRedis :: R.Redis a -> AppM a
runRedis redis = do
   conn <- asks getConnection
   liftIO $ R.runRedis conn redis

server :: ServerT BooksAPI AppM
server = getBooks 
  where getBooks :: AppM [Book]
        getBooks = do
          hello <- runRedis $ do
            R.get "hello"
          case hello of
            Right result -> return [Book $ show result] 
            Left _ -> throwError err500

-- natural transformation from the AppM monad to the Handler
nt :: AppCtx -> AppM a -> Handler a
nt s x = runReaderT x s

app :: AppCtx -> IO Application
app ctx = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck (getConfiguration ctx) (getConnection ctx)
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
  pure $ serveWithContext api cfg $ hoistServer api (nt ctx) server

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
