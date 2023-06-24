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

import AppM ( AppM, getConfiguration, getPool )
import Control.Monad.Trans.Reader  (ReaderT, ask, asks)

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)

import Data.List (break)

data User = User { firstName :: !(Maybe Text)
                 , lastName :: !(Maybe Text)
                 , email :: !(Maybe Text)
                 , username :: !Text
                 , domain :: !(Maybe Text)
                 }
          deriving Show

userDefault :: User
userDefault = User { firstName = Nothing, lastName = Nothing, email = Nothing, username = "", domain = Nothing }

parseUser :: String -> User
parseUser s =
  let (username, domain) = break (== '@') s
  in if null domain
     then userDefault { username = Text.pack username, domain = Nothing }
     else userDefault { username = Text.pack username, domain = Just $ Text.pack $ tail domain }

instance FromJSON User where
    parseJSON (Object v) = f <$> (v .: "id")
                           <*> v .:? "firstName"
                           <*> v .:? "lastName"
                           <*> v .:? "email"
                           where f s fn ln e = (parseUser s) { firstName = fn, lastName = ln, email = e }
    parseJSON _ = pure $ userDefault

instance ToJSON User where
    toJSON (User firstName lastName email username domain) =
      object [ "firstName" .= firstName
             , "lastName" .= lastName
             , "email" .= email
             , "id" .= maybe username (\d -> Text.concat [username, "@", d]) domain
             ]

type GetAPI = Get '[JSON] User 

resolveDomain :: User -> AppM User
resolveDomain u
  | isNothing (domain u) = do
      config <- asks getConfiguration
      pure $ u { domain = Just $ Text.pack $ getHostname config }
  | otherwise = pure u

lookupUser :: Text -> AppM User
lookupUser uid = do
  pool <- asks getPool
  withResource pool $ \conn -> do
    maybeUser <- liftIO $ R.runRedis conn $ R.get $ pack $ "user:" ++ cs uid
    case maybeUser of
      Left _ -> throwError err500
      Right Nothing -> throwError err404
      Right (Just j) -> case eitherDecodeStrict j of
                          Left _ -> throwError err500
                          Right user -> pure user

getUser :: AuthResult AuthenticatedUser -> String -> AppM User 
getUser (Authenticated au) u =
  do
    resolvedUser <- resolveDomain $ parseUser u
    if (A.username au == username resolvedUser) && (Just (A.domain au) == domain resolvedUser)
      then lookupUser $ username resolvedUser 
      else throwError err403
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
