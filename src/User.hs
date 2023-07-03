{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module User (API
            , server
            , UserIdentifier(..)
            , ensureIssuerMatchesUserIdentifier
            ) where

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Server
  
import Configuration
import qualified Database.Redis as R

import Authentication (AuthenticatedUser)
import qualified Authentication as A (AuthenticatedUser( username, domain ))

import Data.Maybe
import Data.Aeson
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.String.Conversions (cs)
import Data.Pool (withResource)
import Control.Monad.Reader
import Control.Monad.Except

import AppM ( AppM, getConfiguration, getPool, MonadDB(..), HasConfiguration(..) )

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)

import Data.List (break)
import qualified Text.Email.Validate as Email
import Data.Aeson.Types (Parser)
  
data UserIdentifier = UserIdentifier { username :: !Text
                                     , domain :: !(Maybe Text)
                                     }
                        deriving Show

parseUserIdentifier :: Text -> Either Text UserIdentifier
parseUserIdentifier text = case Email.emailAddress (cs text) of
      Just address -> Right $ UserIdentifier
                            (cs $ Email.localPart address)
                            (Just $ cs $ Email.domainPart address)
      Nothing -> Right $ UserIdentifier text Nothing

instance FromHttpApiData UserIdentifier where
  parseUrlPiece = parseUserIdentifier 
  
instance FromJSON UserIdentifier where
  parseJSON = withText "UserIdentifier" parse
    where
      parse :: Text -> Parser UserIdentifier
      parse uid = case parseUserIdentifier uid of
        Left e -> fail $ cs e
        Right u -> pure u
  
instance ToHttpApiData UserIdentifier where
  toUrlPiece (UserIdentifier name (Just domain)) =
    Text.concat [ name, "@", domain ]
  toUrlPiece (UserIdentifier name Nothing) = name

instance ToJSON UserIdentifier where
    toJSON (UserIdentifier name (Just domain)) = String $ Text.concat [ name, "@", domain ]
    toJSON (UserIdentifier name Nothing) = String name

data User = User { identifier :: !UserIdentifier
                 , email :: !(Maybe Text)
                 , firstName :: !(Maybe Text)
                 , lastName :: !(Maybe Text)
                 }
          deriving Show

userDefault :: User
userDefault = User { firstName = Nothing, lastName = Nothing, email = Nothing, identifier = UserIdentifier "" Nothing }

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> do
    firstName <- v .:? "firstName"
    lastName <- v .:? "lastName"
    email <- v .:? "email"
    identifier <- v .: "id"
    return $ User { identifier = identifier, firstName = firstName, lastName = lastName, email = email }

instance ToJSON User where
    toJSON (User identifier email firstName lastName) =
      object [ "id" .= identifier
             , "email" .= email
             , "firstName" .= firstName
             , "lastName" .= lastName
             ]

refineUserIdentifier :: (MonadReader r m, HasConfiguration r) => UserIdentifier -> m UserIdentifier
refineUserIdentifier uid@(UserIdentifier _ (Just d)) = pure uid
refineUserIdentifier uid@(UserIdentifier _ Nothing) = do
      config <- asks getConfiguration
      pure $ uid { domain = Just $ Text.pack $ getHostname config }

refineUser :: (MonadReader r m, HasConfiguration r) => User -> m User
refineUser u = do
  i <- refineUserIdentifier (identifier u)
  pure $ u { identifier = i }

thisDomain :: (MonadReader r m, HasConfiguration r) => m Text
thisDomain = do
  config <- asks getConfiguration
  pure $ Text.pack $ getHostname config

ensureIssuerMatchesUserIdentifier :: (MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> m NoContent
ensureIssuerMatchesUserIdentifier (Authenticated au) uid@(UserIdentifier name Nothing) =
  if A.username au == name 
    then pure NoContent
    else throwError err403
ensureIssuerMatchesUserIdentifier (Authenticated au) uid@(UserIdentifier name (Just d)) = do 
  if A.domain au == d
    then ensureIssuerMatchesUserIdentifier (Authenticated au) (UserIdentifier name Nothing)
    else throwError err403
ensureIssuerMatchesUserIdentifier _ _ = throwError err401

lookupUser :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => UserIdentifier -> m User
lookupUser (UserIdentifier name Nothing) = do
  withConnection $ \conn -> do
    maybeUser <- liftIO $ R.runRedis conn $ R.get $ pack $ "user:" ++ cs name
    case maybeUser of
      Left _ -> throwError err500
      Right Nothing -> throwError err404
      Right (Just j) -> case eitherDecodeStrict j of
                          Left _ -> throwError err500
                          Right user -> pure user
lookupUser (UserIdentifier name (Just d)) = do
  d' <- thisDomain
  if d' == d
    then lookupUser (UserIdentifier name Nothing)
    else throwError err404

type GetAPI = Get '[JSON] User 

getUser :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> m User 
getUser (Authenticated au) uid@(UserIdentifier name Nothing) =
  if A.username au == name 
    then lookupUser uid
    else throwError err403
getUser (Authenticated au) uid@(UserIdentifier name (Just d)) = do 
  if A.domain au == d
    then lookupUser uid
    else throwError err403  
getUser _ _ = throwError err401


  
putUser :: (MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> User -> m User
putUser _ _ _ = throwError err401

patchUser ::  (MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> User -> m User
patchUser _ _ _ = throwError err401

type UpdateAPI = ReqBody '[JSON] User :> (Put '[JSON] User :<|> Patch '[JSON] User)

updateUser :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> ServerT UpdateAPI m
updateUser au u uu = putUser au u uu :<|> patchUser au u uu

type API = SAS.Auth '[SA.JWT] AuthenticatedUser :> "users" :> Capture "user" UserIdentifier :>
  ( GetAPI :<|> UpdateAPI )

server :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => SAS.CookieSettings -> SAS.JWTSettings -> ServerT API m
server _ _ au u = getUser au u :<|> updateUser au u
