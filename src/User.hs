{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module User (API, server) where

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Server

import Authentication (AuthenticatedUser)
                      
import Data.Aeson
import Data.Text ( Text )  

import AppM ( AppM )

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)

data User = User { firstName :: !Text
                 , lastName :: !Text
                 , email :: !Text
                 }
          deriving Show

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "firstName"
                                <*> v .: "lastName"
                                <*> v .: "email" 
    parseJSON _ = pure $ User { firstName = "", lastName = "", email = "" } 

instance ToJSON User where
    toJSON (User firstName lastName email) = object ["firstName" .= firstName, "lastName" .= lastName, "email" .= email]
    
getUser :: AuthResult AuthenticatedUser -> AppM User 
getUser _ = throwError err401

putUser :: User -> AppM User
putUser _ = throwError err401

patchUser :: User -> AppM User
patchUser _ = throwError err401

type UpdateAPI = ReqBody '[JSON] User :> (Put '[JSON] User :<|> Patch '[JSON] User)
type GetAPI = Get '[JSON] User 
type API = SAS.Auth '[SA.JWT] AuthenticatedUser :> GetAPI -- :<|> UpdateAPI)

updateUser :: ServerT UpdateAPI AppM 
updateUser u = putUser u :<|> patchUser u

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API AppM
server _ _ u = getUser u
  -- :<|> updateUser
