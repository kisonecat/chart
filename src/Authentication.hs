{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Authentication ( API, server,
                        AuthenticatedUser(..), authCheck ) where

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

import Control.Monad.Reader
import Control.Monad.Except

import Crypto.JWT (signClaims, SignedJWT, ClaimsSet, emptyClaimsSet, Audience(..))
import Crypto.JWT (claimAud, claimIss, claimSub)
import Crypto.JWT (stringOrUri, string, uri)

import qualified Text.Email.Validate as Email

import AppM ( AppM, AppCtx, HasConfiguration(..), MonadDB(..) )

import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import Control.Monad (when)

import Crypto.BCrypt
import Text.StringRandom (stringRandomIO)
import Data.Pool (withResource)
import Control.Lens

import Network.URI
import qualified Text.Email.Validate as Email

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
      (u,d) <- case Email.emailAddress . cs =<< iss of
        Just address -> Right (cs $ Email.localPart address, cs $ Email.domainPart address)
        Nothing -> Left "Invalid issuer"
      pure $ AuthenticatedUser { username = u
                               , domain = d
                               , subject = sub
                               , audience = auds
                               }
              
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

instance MimeUnrender JSONWebToken SignedJWT where
  mimeUnrender _ bs = case decodeCompact bs of
    Left err -> Left . show $ (err :: Error)
    Right jwt -> Right jwt

type instance BasicAuthCfg = BasicAuthData -> IO (BasicAuthResult AuthenticatedUser)

--instance FromBasicAuthData AuthenticatedUser where
--  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type API = "users" :> Capture "user" String :> ( Post '[JSON] ()  :<|> (Servant.BasicAuth "doenet" AuthenticatedUser :> "token" :> Get '[JSONWebToken] SignedJWT)  :<|> ( Servant.BasicAuth "doenet" AuthenticatedUser :> "authorize" :> Get '[JSON] () ) )

authCheck :: Configuration -> R.Connection
          -> BasicAuthCheck AuthenticatedUser
authCheck config conn = BasicAuthCheck $ \(BasicAuthData login password) -> do
  maybeHashed <- R.runRedis conn $ R.get $ pack ("pw:" ++ cs login)
  pure $ case maybeHashed of
    Left _ -> Servant.Unauthorized
    Right Nothing -> Servant.NoSuchUser
    Right (Just hashed) -> if validatePassword hashed password
      then Servant.Authorized AuthenticatedUser { username = cs login
                                                , domain = cs $ getHostname config
                                                , subject = Nothing
                                                , audience = Nothing
                                                }
      else Servant.BadPassword

bestAlg :: (MonadRandom m) => AuthenticatedUser -> SAS.JWTSettings -> m (Either Crypto.JOSE.Error Crypto.JWT.SignedJWT)
bestAlg u jwt  = runExceptT $ do
  let jwk = signingKey jwt
  alg <- JOSE.bestJWSAlg jwk
  let claims = encodeJWT u
  signClaims jwk (newJWSHeader ((), alg)) claims

userToken :: (MonadRandom m, MonadError ServerError m) => SAS.JWTSettings ->  String -> AuthenticatedUser ->m SignedJWT 
userToken jwt _user u = do
  --let jwk = signingKey jwt
  --alg <- liftIO $ bestAlg jwt 
  --let claims = encodeJWT u
  --signed <- signClaims jwk (newJWSHeader ((), alg)) claims
  signed <- bestAlg u jwt
  case signed of
     Left _ -> throwError err500
     -- Right j -> pure $ Book $ cs $ encodeCompact j
     Right j -> pure j
  
userToken _ _ _ = throwError err401
  
userAuthorize :: (MonadError ServerError m) => String -> AuthenticatedUser -> m () 
userAuthorize _adminAuthenticatedUser _user = pure () 
userAuthorize _ _ = throwError err401

postUser :: (MonadIO m, MonadError ServerError m, MonadDB m) => String -> m () 
postUser email =
  if not $ Email.isValid (cs email)
    then throwError err401 { errBody = "Invalid email" }
    else do
      name <- liftIO $ stringRandomIO "[a-z]{16}"
      password <- liftIO $ stringRandomIO "[a-z]{16}"
      hashed <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (cs password)
      _ <- case hashed of
        Nothing -> throwError err401 { errBody = "Could not calculate hash" }
        Just hashed' ->
          rset (cs ("pw:" ++ cs name)) hashed'

      -- should email with the given address
      liftIO $ putStrLn $ "username: " ++ cs name ++ " password: " ++ cs password
      
      pure ()
  
postUser _ = throwError err401

server :: (MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => SAS.CookieSettings -> SAS.JWTSettings -> ServerT API m
server _cookies jwt u = postUser u :<|> userToken jwt u :<|> userAuthorize u 
