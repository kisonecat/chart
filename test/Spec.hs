{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import           Control.Exception                (bracket)
--import           Control.Lens              hiding (Context)
import           Data.Aeson
--import qualified Data.HashMap.Strict              as HM
--import           Data.Text                        (Text, unpack)
--import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client
import           Servant.Server
--import           Servant.QuickCheck
--import           Servant.QuickCheck.Internal (serverDoesntSatisfy)
import Servant.Auth.Server as SAS
import Crypto.Hash (Digest, SHA256(SHA256))
  
import Authentication (AuthenticatedUser(..))
  
import qualified Authentication (API)
import Crypto.JWT (SignedJWT)
  
import User (UserIdentifier(..), User(..))
import qualified User (API)
import Servant.Auth.Client (Token(..))

import qualified Learner (API)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher

import Crypto.JWT

import App (TheAPI, server, testApplication)
import Network.Wai.Handler.Warp
    (setLogger, setPort, getPort, runSettings, defaultSettings )
import Data.ByteString.Char8 (pack)
  
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.String.Conversions (cs)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Network.URI
import Data.Maybe (fromJust)
  
data UserClient = UserClient 
  { authenticationClient :: [Char] -> AuthenticationClient
  }

data AuthenticationClient = AuthenticationClient 
  { postUser :: ClientM ()
  , getJWT :: BasicAuthData -> ClientM SignedJWT
  , getAuthorize :: BasicAuthData -> ClientM ()
  }

-- mkUserClient :: UserClient
-- mkUserClient = UserClient{..}
--   where
--     api = Proxy :: Proxy Authentication.API 
--     authenticationClient = client api
--     mkAuthenticationClient t uid = AuthenticationClient{..}
--       where
--         postUser :<|> (getToken :<|> getAuthorize) = authenticationClient t uid
  
mkAuthenticationClient :: String -> AuthenticationClient
mkAuthenticationClient uid = AuthenticationClient{..}
   where
     api = Proxy :: Proxy Authentication.API 
     authenticationClient = client api
     postUser :<|> (getJWT :<|> getAuthorize) = authenticationClient uid
    
data LearnerClient = LearnerClient 
  { redirectToWorksheet :: Token -> Digest SHA256 -> URI -> ClientM NoContent
  , mkProgressStateClient :: Token -> UserIdentifier -> Digest SHA256 -> URI -> ProgressStateClient
  }

data ProgressStateClient = ProgressStateClient
  { putProgress :: Double -> ClientM NoContent
  , getProgress :: ClientM Double
  , putState :: Value -> ClientM NoContent
  , getState :: ClientM Value
  }

mkLearnerClient :: LearnerClient
mkLearnerClient = LearnerClient{..}
  where
    api = Proxy :: Proxy Learner.API 
    redirectToWorksheet :<|> progressStateClient = client api
    mkProgressStateClient t uid hash uri = ProgressStateClient{..}
      where
        progressClient :<|> stateClient = progressStateClient t uid hash uri
        putProgress :<|> getProgress = progressClient
        putState :<|> getState = stateClient
  
data ProfileUpdateClient = ProfileUpdateClient 
  { putUser :: ClientM User 
  , patchUser :: ClientM User 
  }
  
data ProfileClient = ProfileClient 
  { getUser :: ClientM User
  , mkProfileUpdateClient :: User -> ProfileUpdateClient
  }

mkProfileClient :: Token -> UserIdentifier -> ProfileClient
mkProfileClient token uid = ProfileClient{..}
   where
     api = Proxy :: Proxy User.API 
     getUser :<|> updateClient = client api token uid
     mkProfileUpdateClient user = ProfileUpdateClient{..}
       where
         putUser :<|> patchUser = updateClient user

getUser' :: Token -> UserIdentifier -> ClientM User
getUser' token uid = getUser (mkProfileClient token uid)
  
putUser' :: Token -> UserIdentifier -> User -> ClientM User
putUser' token uid user = putUser (mkProfileUpdateClient (mkProfileClient token uid) user)
  
patchUser' :: Token -> UserIdentifier -> User -> ClientM User
patchUser' token uid user = patchUser (mkProfileUpdateClient (mkProfileClient token uid) user)

  
    
withApp :: (Warp.Port -> IO a) -> IO a
withApp action = do
    let app = testApplication
    Warp.testWithApplication app $ \port -> action port

try :: Warp.Port -> ClientM a -> IO (Either ClientError a)
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
  runClientM action (clientEnv port)
  
businessLogicSpec :: Spec
businessLogicSpec =
  around withApp $ do
    describe "POST /user" $ do
      it "should create a user with a high enough ID" $ \port -> do
        result <- try port $ postUser $ mkAuthenticationClient "test@test.com" 
        case result of
          Left err -> error $ show err
          Right _ -> return ()

tokenForUser :: Text -> ClientM Token
tokenForUser name = do
  signedJWT <- (getJWT $ mkAuthenticationClient $ cs name)
    BasicAuthData { basicAuthUsername = cs name, basicAuthPassword = "password" }
  pure $ Token { getToken = cs $ encodeCompact signedJWT }
  
userProfileSpec :: Spec
userProfileSpec =
  around withApp $ do
    describe "GET /users" $ do
      it "provides a blank profile for a new user" $ \port -> do
        let name = "test"
        profile <- try port $ do
          token <- tokenForUser name
          getUser' token $ UserIdentifier { username = name, domain = Nothing }
        case profile of
          Left err -> error $ show err
          Right p -> do
            identifier p `shouldBe` UserIdentifier { username = name, domain = Just "localhost" }
            email p `shouldBe` Nothing
            firstName p `shouldBe` Nothing
            lastName p `shouldBe` Nothing
        return ()
  
      it "saves the user profile with PUT" $ \port -> do
        let name = "test"
        let uid = UserIdentifier { username = name, domain = Just "localhost" }
        let user = User { identifier = uid
                        , firstName = Just "first name"
                        , lastName = Just "last name"
                        , email = Just "my-email@email.com"
                        }
        profile <- try port $ do
          token <- tokenForUser name
          putUser' token UserIdentifier { username = name, domain = Nothing } user
          getUser' token uid
        case profile of
          Left err -> error $ show err
          Right p -> do
            p `shouldBe` user
        return ()
  
      it "updates the user profile with PATCH" $ \port -> do
        let name = "test"
        let uid = UserIdentifier { username = name, domain = Just "localhost" }
        let user = User { identifier = uid
                        , firstName = Just "first name"
                        , lastName = Just "last name"
                        , email = Just "my-email@email.com"
                        }
        let patch = User { identifier = uid
                         , firstName = Just "new name"
                         , lastName = Nothing
                         , email = Nothing }
        profile <- try port $ do
          token <- tokenForUser name
          putUser' token UserIdentifier { username = name, domain = Nothing } user
          patchUser' token UserIdentifier { username = name, domain = Nothing } patch
          getUser' token uid
        case profile of
          Left err -> error $ show err
          Right p -> do
            identifier p `shouldBe` identifier user
            firstName p `shouldBe` firstName patch
            email p `shouldBe` email user
            lastName p `shouldBe` lastName user
        return ()
         
spec :: Spec
spec = do
  around withApp $ do
    describe "GET /learners" $ do
      it "progress is initialized to zero" $ \port -> do
        result <- try port $ (getJWT $ mkAuthenticationClient "test") BasicAuthData { basicAuthUsername = "test", basicAuthPassword = "password" }
        let uid = UserIdentifier { username = "test"
                                 , domain = Nothing
                                 }
        let url = fromJust $ parseURI "https://example.com/worksheet"
        let digest = hashWith SHA256 (pack $ uriToString id url "")
        progress <- case result of
          Left err -> error $ show err
          Right signedJWT ->
            let token = Token { getToken = cs $ encodeCompact signedJWT }
            in try port $ getProgress $ ((mkProgressStateClient mkLearnerClient) token uid digest url)
        progress' <- case progress of
          Left err -> error $ show err
          Right x -> pure x
        progress' `shouldBe` 0
  
      it "progress can be set and read" $ \port -> do
        result <- try port $ (getJWT $ mkAuthenticationClient "test") BasicAuthData { basicAuthUsername = "test", basicAuthPassword = "password" }
        let uid = UserIdentifier { username = "test"
                                 , domain = Nothing
                                 }
        let url = fromJust $ parseURI "https://example.com/worksheet"
        let digest = hashWith SHA256 (pack $ uriToString id url "")
        let url' = fromJust $ parseURI "https://example.com/worksheet2"
        let digest' = hashWith SHA256 (pack $ uriToString id url' "")
        progress <- case result of
          Left err -> error $ show err
          Right signedJWT ->
            let token = Token { getToken = cs $ encodeCompact signedJWT }
            in do
              try port $ do
                putProgress ((mkProgressStateClient mkLearnerClient) token uid digest url) 0.17
                putProgress ((mkProgressStateClient mkLearnerClient) token uid digest' url') 0.17
                getProgress $ ((mkProgressStateClient mkLearnerClient) token uid digest url)
                getProgress $ ((mkProgressStateClient mkLearnerClient) token uid digest url)
        progress' <- case progress of
          Left err -> error $ show err
          Right x -> pure x
        progress' `shouldBe` 0.17


main :: IO ()
main = hspec $ do
  userProfileSpec
  spec
  businessLogicSpec
