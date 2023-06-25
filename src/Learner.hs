{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-} 

module Learner (API, server) where

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Server

import Network.URI (URI, parseURI, isURI, uriToString)

  
import Configuration
import qualified Database.Redis as R

import Authentication (AuthenticatedUser)
import qualified Authentication as A (AuthenticatedUser(..))

import Data.Maybe
import Data.Aeson
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.String.Conversions (cs)
import Data.Pool (withResource)

import AppM ( AppM, getConfiguration, getPool )
import Control.Monad.Trans.Reader  (ReaderT, ask, asks)

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)

instance FromHttpApiData URI where
  parseUrlPiece text =
    case parseURI (cs text) of
      Just uri -> Right uri
      _ -> Left "Invalid URI"

instance ToHttpApiData URI where
  toUrlPiece :: URI -> Text
  toUrlPiece uri = cs (uriToString id uri "")

redirectToWorksheet :: AuthResult AuthenticatedUser -> String -> URI -> AppM NoContent
redirectToWorksheet (Authenticated au) hash worksheet = throwError err401 { errBody = "" }

type WorksheetAPI = SAS.Auth '[SA.JWT] AuthenticatedUser :> "worksheets" :> Capture "worksheet" String :> "token" :> Header' '[Required, Strict] "Worksheet" URI :> Post '[JSON] NoContent 
  
worksheetServer :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT WorksheetAPI AppM
worksheetServer _ _ au hash worksheet = redirectToWorksheet au hash worksheet

type ProgressAPI = "progress" :> ((ReqBody '[JSON] Double :> Put '[JSON] NoContent) :<|> Get '[JSON] Double)

getProgress :: AuthResult AuthenticatedUser -> String -> String -> URI -> AppM Double
getProgress = undefined
  
putProgress :: AuthResult AuthenticatedUser -> String -> String -> URI -> Double -> AppM NoContent
putProgress = undefined
  
progressServer :: AuthResult AuthenticatedUser -> String -> String -> URI -> ServerT ProgressAPI AppM
progressServer au learner hash worksheet  =
  putProgress au learner hash worksheet :<|>
  getProgress au learner hash worksheet
  
type StateAPI = "state" :> ((ReqBody '[JSON] Value :> Put '[JSON] NoContent) :<|> Get '[JSON] Value)

getState :: AuthResult AuthenticatedUser -> String -> String -> URI -> AppM Value
getState = undefined
  
putState :: AuthResult AuthenticatedUser -> String -> String -> URI -> Value -> AppM NoContent
putState = undefined
  
stateServer :: AuthResult AuthenticatedUser -> String -> String -> URI -> ServerT StateAPI AppM
stateServer au learner hash worksheet =
  putState au learner hash worksheet :<|>
  getState au learner hash worksheet
  
type LearnerAPI = SAS.Auth '[SA.JWT] AuthenticatedUser :> "learners" :> Capture "learner" String :> "worksheets" :> Capture "worksheet" String :> Header' '[Required, Strict] "Worksheet" URI :> (ProgressAPI :<|> StateAPI)

learnerServer :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT LearnerAPI AppM
learnerServer _ _ au learner hash worksheet =
  progressServer au learner hash worksheet :<|>
  stateServer au learner hash worksheet
  
type API = WorksheetAPI :<|> LearnerAPI
                                                                                           
server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API AppM
server cookie jwt = worksheetServer cookie jwt :<|> learnerServer cookie jwt 
