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
import qualified Data.Text as Text

import AppM ( AppM )

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)

data User = User { firstName :: !Text
                 , lastName :: !Text
                 , email :: !Text
                 , username :: !Text
                 , domain :: !Text
                 }
          deriving Show

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "firstName"
                                <*> v .: "lastName"
                                <*> v .: "email"
                                <*> username
                                <*> domain
      where username = v .: "id"
            domain = v .: "id"
    parseJSON _ = pure $ User { firstName = ""
                              , lastName = ""
                              , email = ""
                              , username = ""
                              , domain = ""
                              } 

instance ToJSON User where
    toJSON (User firstName lastName email username domain) =
      object [ "firstName" .= firstName
             , "lastName" .= lastName
             , "email" .= email
             , "id" .= Text.concat [ username, "@", domain ] 
             ]

type GetAPI = Get '[JSON] User 
    
getUser :: AuthResult AuthenticatedUser -> String -> AppM User 
getUser (Authenticated au) u = pure $ User { firstName = ""
                                           , lastName = ""
                                           , email = ""
                                           , username = ""
                                           , domain = ""
                                           }
                             
getUser _ _ = throwError err401 { errBody = "" }

putUser :: AuthResult AuthenticatedUser -> String -> User -> AppM User
putUser _ _ _ = throwError err401

patchUser :: AuthResult AuthenticatedUser -> String -> User -> AppM User
patchUser _ _ _ = throwError err401

type UpdateAPI = ReqBody '[JSON] User :> (Put '[JSON] User :<|> Patch '[JSON] User)

updateUser :: AuthResult AuthenticatedUser -> String -> ServerT UpdateAPI AppM 
updateUser au u uu = putUser au u uu :<|> patchUser au u uu

type API = SAS.Auth '[SA.JWT] AuthenticatedUser :> "users" :> Capture "user" String :> ( GetAPI :<|> UpdateAPI )

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API AppM
server _ _ au u = getUser au u :<|> updateUser au u
