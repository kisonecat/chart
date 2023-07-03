{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-} 

module Learner (API, server) where

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Server

import Network.URI
import User (UserIdentifier(..), ensureIssuerMatchesUserIdentifier)

import Codec.Compression.GZip (compress, decompress)

import Configuration
import qualified Database.Redis as R

import Authentication (AuthenticatedUser(..))

import Data.Maybe
import Data.Aeson
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.String.Conversions (cs)
import Data.Pool (withResource)

import Crypto.JWT (signClaims, SignedJWT, ClaimsSet, emptyClaimsSet, Audience(..))
import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE

import AppM ( AppM, MonadDB(..), HasConfiguration(..), HasSymmetricJWK(..) )

import           Control.Monad.Except        (liftEither, throwError, runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.Except

import Crypto.Hash (Digest, SHA256(SHA256), hashWith, digestFromByteString)

import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteArray.Encoding (convertFromBase, convertToBase, Base(Base16))
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Control.Exception (try, IOException, SomeException)
import qualified Data.ByteString.Lazy as BL

compressWithException :: BL.ByteString -> IO (Either SomeException BL.ByteString)
compressWithException bs = try $ return $ compress bs

decompressWithException :: BL.ByteString -> IO (Either SomeException BL.ByteString)
decompressWithException bs = try $ return $ decompress bs

instance FromHttpApiData URI where
  parseUrlPiece text =
    case parseURI (cs text) of
      Just uri -> Right uri
      _ -> Left "Invalid URI"

instance ToHttpApiData URI where
  toUrlPiece :: URI -> Text
  toUrlPiece uri = cs (uriToString id uri "")

digestToHex :: Digest SHA256 -> Text
digestToHex digest = decodeUtf8 $ convertToBase Base16 (digestToByteString digest)
    where digestToByteString :: Digest SHA256 -> ByteString
          digestToByteString = convert
    
instance ToHttpApiData (Digest SHA256) where
  toUrlPiece :: Digest SHA256 -> Text
  toUrlPiece = digestToHex
  
instance FromHttpApiData (Digest SHA256) where
  parseUrlPiece :: Text -> Either Text (Digest SHA256)
  parseUrlPiece text =
    let c :: Either String ByteString = convertFromBase Base16 (encodeUtf8 text)
    in case c of
      Right bs -> case digestFromByteString bs of
                    Just digest -> Right digest
                    Nothing -> Left "Invalid SHA256 digest"
      Left _ -> Left "Invalid SHA256 digest"
  
uriToDigest :: URI -> Digest SHA256
uriToDigest uri = hashWith SHA256 $ pack $ uriToString id uri ""

ensureDigestMatches :: (MonadIO m, MonadDB m, MonadError ServerError m) => Digest SHA256 -> URI -> m NoContent
ensureDigestMatches digest uri = 
  if uriToDigest uri == digest
  then pure NoContent
  else throwError err400 { errBody = "Digest does not match the provided URI" }

appendQuery :: String -> String -> URI -> URI
appendQuery key value uri = 
    uri { uriQuery = newQuery }
    where
        newQuery = oldQuery ++ (if null oldQuery then "?" else "&") ++
          key ++ "=" ++ escapeURIString isUnescapedInURIComponent value
        oldQuery = uriQuery uri

bestAlg :: AuthenticatedUser -> SAS.JWTSettings -> JWK -> IO (Either Crypto.JOSE.Error Crypto.JWT.SignedJWT)
bestAlg u jwt jwk = runExceptT $ do
  --let jwk = signingKey jwt
  alg <- JOSE.bestJWSAlg jwk
  let claims = encodeJWT u
  signClaims jwk (newJWSHeader ((), alg)) claims

redirectToWorksheet :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasSymmetricJWK r, MonadError ServerError m) => SAS.JWTSettings -> AuthResult AuthenticatedUser -> Digest SHA256 -> URI -> m NoContent
redirectToWorksheet cfg (Authenticated au) digest uri = do
  key <- asks getSymmetricJWK
  let au' = au { subject = cs . uriRegName <$> uriAuthority uri }
  signed <- liftIO $ bestAlg au cfg key
  case signed of
    Left _ -> throwError err500
    Right j -> do 
      let jwt = encodeCompact j
      let uri' = appendQuery "jwt" (cs jwt) uri
      throwError err303  { errHeaders = [("Location", cs $ uriToString id uri' "")] }
redirectToWorksheet _ _ _ _ = throwError err401

type WorksheetAPI = SAS.Auth '[SA.JWT] AuthenticatedUser :> "worksheets" :> Capture "worksheet" (Digest SHA256) :> "token" :> Header' '[Required, Strict] "Worksheet" URI :> Post '[JSON] NoContent 
  
worksheetServer :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasSymmetricJWK r, MonadError ServerError m) => SAS.CookieSettings -> SAS.JWTSettings -> ServerT WorksheetAPI m
worksheetServer _ = redirectToWorksheet

type ProgressAPI = "progress" :> ((ReqBody '[JSON] Double :> Put '[JSON] NoContent) :<|> Get '[JSON] Double)

getProgress :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> Digest SHA256 -> URI -> m Double
getProgress au uid@(UserIdentifier name _) digest uri = do
  ensureDigestMatches digest uri
  ensureIssuerMatchesUserIdentifier au uid
  withConnection $ \conn -> do
    let key = pack $ "score:" ++ cs name
    d <- liftIO $ R.runRedis conn $ R.zscore key (convert digest)
    case d of
      Left err -> throwError err500 { errBody = cs $ show err } 
      Right Nothing -> pure 0.0
      Right (Just score) -> pure score
  
putProgress :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> Digest SHA256 -> URI -> Double -> m NoContent
putProgress au uid@(UserIdentifier name _) digest uri score = do
  ensureDigestMatches digest uri
  ensureIssuerMatchesUserIdentifier au uid
  withConnection $ \conn -> do
    let key = pack $ "score:" ++ cs name
    d <- liftIO $ R.runRedis conn $ R.zadd key [(score, convert digest)]
    case d of
      Left err -> throwError err500 { errBody = cs $ show err } 
      Right _ -> pure NoContent
  
progressServer :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> Digest SHA256 -> URI -> ServerT ProgressAPI m
progressServer au learner hash worksheet  =
  putProgress au learner hash worksheet :<|>
  getProgress au learner hash worksheet
  
type StateAPI = "state" :> ((ReqBody '[JSON] Value :> Put '[JSON] NoContent) :<|> Get '[JSON] Value)

getState :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> Digest SHA256 -> URI -> m Value
getState au uid@(UserIdentifier name _) digest uri = do
  ensureDigestMatches digest uri
  ensureIssuerMatchesUserIdentifier au uid
  withConnection $ \conn -> do
    let key = pack $ "state:" ++ cs name
    d <- liftIO $ R.runRedis conn $ R.hget key (convert digest)
    case d of
      Left err -> throwError err500 { errBody = cs $ show err } 
      Right Nothing -> pure $ toJSON ([] :: [Int])
      Right (Just bs) -> case decode $ decompress $ cs bs of
        Nothing -> throwError err500
        Just v -> pure v
  
putState :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> Digest SHA256 -> URI -> Value -> m NoContent
putState au uid@(UserIdentifier name _) digest uri value = do
  ensureDigestMatches digest uri
  ensureIssuerMatchesUserIdentifier au uid
  withConnection $ \conn -> do
    let key = pack $ "state:" ++ cs name
    let compressedValue = cs $ compress $ encode value
    d <- liftIO $ R.runRedis conn $ R.hset key (convert digest) compressedValue
    case d of
      Left err -> throwError err500 { errBody = cs $ show err } 
      Right _ -> pure NoContent
  
stateServer ::(MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => AuthResult AuthenticatedUser -> UserIdentifier -> Digest SHA256 -> URI -> ServerT StateAPI m
stateServer au learner hash worksheet =
  putState au learner hash worksheet :<|>
  getState au learner hash worksheet
  
type LearnerAPI = SAS.Auth '[SA.JWT] AuthenticatedUser :> "learners" :> Capture "learner" UserIdentifier :> "worksheets" :> Capture "worksheet" (Digest SHA256) :> Header' '[Required, Strict] "Worksheet" URI :> (ProgressAPI :<|> StateAPI)

learnerServer :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, MonadError ServerError m) => SAS.CookieSettings -> SAS.JWTSettings -> ServerT LearnerAPI m
learnerServer _ _ au learner hash worksheet =
  progressServer au learner hash worksheet :<|>
  stateServer au learner hash worksheet
  
type API = WorksheetAPI :<|> LearnerAPI
                                                                                           
server :: (MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r, HasSymmetricJWK r, MonadError ServerError m) => SAS.CookieSettings -> SAS.JWTSettings -> ServerT API m
server cookie jwt = worksheetServer cookie jwt :<|> learnerServer cookie jwt 

