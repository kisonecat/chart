{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Authentication ( API, server, AuthenticatedUser(username, domain), authCheck ) where

import GHC.Generics ( Generic )
import Data.Text ( Text, unpack )  
import Data.Aeson
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (listToMaybe)

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Server

import Configuration
import qualified Database.Redis as R
import Data.String.Conversions (cs)
import Data.ByteString.Char8 (pack)

import Network.HTTP.Media ((//), (/:))

import Control.Monad.Trans.Reader  (ReaderT, ask, asks)

import Crypto.JWT (signClaims, SignedJWT, ClaimsSet, emptyClaimsSet, Audience(..))
import Crypto.JWT (claimAud, claimIss, claimSub)
import Crypto.JWT (stringOrUri, string)

import qualified Text.Email.Validate as Email

import AppM ( AppM, AppCtx, getPool )

import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import Control.Monad (when)

import Crypto.BCrypt
import Text.StringRandom (stringRandomIO)
import Data.Pool (withResource)
import Control.Lens

data AuthenticatedUser = AuthenticatedUser { username :: !Text
                                           , domain :: !Text
                                           , subject :: !(Maybe Text)
                                           , audience :: !(Maybe Text)
                                           }
                       deriving (Show, Generic)

instance FromJSON AuthenticatedUser
instance ToJSON AuthenticatedUser

instance FromJWT AuthenticatedUser where
    decodeJWT claims = do
      let iss = preview string =<< view claimIss claims
      let sub = preview string =<< view claimSub claims
      let auds = preview string =<< (\(Audience xs) -> listToMaybe xs) =<< view claimAud claims
      let (u, d) = maybe ("","") splitUsernameAndDomain iss
      when (u == "" || d == "") $
        Left "Invalid username or domain"
      pure $ AuthenticatedUser { username = u
                               , domain = d
                               , subject = sub
                               , audience = auds
                               }
      where
        splitUsernameAndDomain :: Text -> (Text, Text)
        splitUsernameAndDomain s =
          let (u, d) = break (== '@') (cs s)
          in (cs u, cs d)
              
instance ToJWT AuthenticatedUser where
    encodeJWT user =
      let issuer = preview stringOrUri (username user <> "@" <> domain user)
          sub = preview stringOrUri =<< subject user
          aud = preview stringOrUri =<< audience user
      in
        emptyClaimsSet & claimIss .~ issuer
                       & claimSub .~ sub
                       & claimAud .~ fmap (\a -> Audience [a]) aud

data JSONWebToken  
  
instance Accept JSONWebToken where
   contentType _ = "application" // "jwt" 

instance {-# OVERLAPPING #-} ToCompact a => MimeRender JSONWebToken a where
    mimeRender _ = encodeCompact

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type API = SAS.Auth '[SA.BasicAuth] AuthenticatedUser :> "users" :> Capture "user" String :> ( Post '[JSON] ()  :<|> "token" :> Get '[JSONWebToken] SignedJWT  :<|> "authorize" :> Get '[JSON] () )

authCheck :: Configuration -> R.Connection
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck config conn (BasicAuthData login password) = do
  maybeHashed <- R.runRedis conn $ R.get $ pack ("pw:" ++ cs login)
  pure $ case maybeHashed of
    Left _ -> SAS.Indefinite
    Right Nothing -> SAS.NoSuchUser
    Right (Just hashed) -> if validatePassword hashed password
      then Authenticated AuthenticatedUser { username = cs login
                                           , domain = cs $ getHostname config
                                           , subject = Nothing
                                           , audience = Nothing
                                           }
      else SAS.BadPassword

bestAlg :: AuthenticatedUser -> SAS.JWTSettings -> IO (Either Crypto.JOSE.Error Crypto.JWT.SignedJWT)
bestAlg u jwt  = runExceptT $ do
  let jwk = signingKey jwt
  alg <- JOSE.bestJWSAlg jwk
  let claims = encodeJWT u
  signClaims jwk (newJWSHeader ((), alg)) claims

userToken :: SAS.JWTSettings -> AuthResult AuthenticatedUser -> String -> AppM SignedJWT 
userToken jwt (SAS.Authenticated u) _user = do
  --let jwk = signingKey jwt
  --alg <- liftIO $ bestAlg jwt 
  --let claims = encodeJWT u
  --signed <- signClaims jwk (newJWSHeader ((), alg)) claims
  signed <- liftIO $ bestAlg u jwt
  case signed of
     Left _ -> throwError err500
     -- Right j -> pure $ Book $ cs $ encodeCompact j
     Right j -> pure j
  
userToken _ _ _ = throwError err401
  
userAuthorize :: AuthResult AuthenticatedUser -> String -> AppM () 
userAuthorize (SAS.Authenticated _adminAuthenticatedUser) _user = pure () 
userAuthorize _ _ = throwError err401

postUser :: AuthResult AuthenticatedUser -> String -> AppM () 
postUser SAS.Indefinite email =
  if not $ Email.isValid (cs email)
    then throwError err401 { errBody = "Invalid email" }
    else do
      pool <- asks getPool
      withResource pool $ \conn -> do
        name <- liftIO $ stringRandomIO "[a-z]{16}"
        password <- liftIO $ stringRandomIO "[a-z]{16}"
        hashed <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (cs password)
        _ <- case hashed of
          Nothing -> throwError err401 { errBody = "Could not calculate hash" }
          Just hashed' ->
            liftIO $ R.runRedis conn $ R.set (cs ("pw:" ++ cs name)) hashed'

        -- should email with the given address
        liftIO $ putStrLn $ "username: " ++ cs name ++ " password: " ++ cs password
      
      pure ()
  
postUser _ _ = throwError err401

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API AppM
server _cookies jwt u v = postUser u v :<|> userToken jwt u v :<|> userAuthorize u v 
