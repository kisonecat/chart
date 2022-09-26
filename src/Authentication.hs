{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Authentication ( API, server, AuthenticatedUser, authCheck ) where

import GHC.Generics ( Generic )
import Data.Text ( Text )  
import Data.Aeson

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Server

import Configuration
import qualified Database.Redis as R
import Data.String.Conversions (cs)

import Network.HTTP.Media ((//), (/:))

import Crypto.JWT (signClaims, SignedJWT)

import AppM ( AppM )

import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)

data AuthenticatedUser = AuthenticatedUser { name :: !Text
                                           , chart :: !Text
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

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type API = SAS.Auth '[SA.BasicAuth] AuthenticatedUser :> ( "token" :> Get '[JSONWebToken] SignedJWT  :<|> "authorize" :> Get '[JSON] () )

authCheck :: Configuration -> R.Connection
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck config _connection (BasicAuthData login _password) = pure $
  maybe SAS.Indefinite Authenticated $ Just $ AuthenticatedUser { name = cs login, chart = cs $ getHostname config }

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

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API AppM
server _cookies jwt u = userToken jwt u :<|> userAuthorize u

