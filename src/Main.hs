{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where


import Configuration
    ( defaultConfiguration,
      updateGithubAccessToken,
      updateGithubRoot,
      Configuration )

import Servant
    ( serve,
      type (:>),
      Get,
      HasServer(ServerT),
      Proxy(..),
      Application,
      Handler,
      hoistServer,
      JSON ) 

import Servant.Auth as SA
import Servant.Auth.Server as SA
  
import           Control.Concurrent          (forkIO, killThread)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)

import           Control.Exception           (bracket)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)

import           GHC.Generics                (Generic)
import           Network.Wai.Handler.Warp    (run)

import System.Environment (lookupEnv)

import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp
    (setLogger, setPort, getPort, runSettings, defaultSettings )
import Configuration.Dotenv (loadFile, defaultConfig)

import           Data.Aeson                  (FromJSON, ToJSON)
import Data.Text  

data User = User { username :: !Text
                 , firstName :: !Text
                 , lastName :: !Text
                 , email :: !Text
                 }
          deriving (Show, Generic)

instance FromJSON User
instance ToJSON User 

newtype Book = Book String deriving (Show, Generic)
instance ToJSON Book
instance FromJSON Book

type GetBooks = Get '[JSON] [Book]
type BooksAPI = "books" :> (GetBooks)

api :: Proxy BooksAPI
api = Proxy
                       
type AppM = ReaderT Configuration Handler

server :: ServerT BooksAPI AppM
server = getBooks 
  where getBooks :: AppM [Book]
        getBooks = do
          return []

-- natural transformation from the AppM monad to the Handler
nt :: Configuration -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Configuration -> Application
app s = serve api $ hoistServer api (nt s) server

main :: IO ()
main = do
  -- with this, lookupEnv will fetch from .env or from an environment variable
  _ <- Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  port <- lookupEnv "PORT"
  let settings = maybe id (setPort . read) port $ defaultSettings
  
  root <- lookupEnv "GITHUB_ROOT"
  accessToken <- lookupEnv "GITHUB_ACCESS_TOKEN"
  let config = updateGithubRoot root $ updateGithubAccessToken accessToken $ defaultConfiguration

  putStrLn $ "Listening on port " ++ show (getPort settings)

  runSettings settings $ app config
