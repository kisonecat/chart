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
  
import User (UserIdentifier(..))
import Servant.Auth.Client
import qualified Learner (API)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher

import App (TheAPI, server, testApplication)
import Network.Wai.Handler.Warp
    (setLogger, setPort, getPort, runSettings, defaultSettings )

data UserClient = UserClient 
  { authenticationClient :: [Char] -> AuthenticationClient
  }

data AuthenticationClient = AuthenticationClient 
  { postUser :: ClientM ()
  , getToken :: BasicAuthData -> ClientM SignedJWT
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
     postUser :<|> (getToken :<|> getAuthorize) = authenticationClient uid
    
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
    --let p = postUser . authenticationClient $ mkUserClient
    --let pt = 8080
    -- testing scenarios start here
    describe "POST /user" $ do
      it "should create a user with a high enough ID" $ \port -> do
        result <- try port $ postUser $ mkAuthenticationClient "test@test.com" 
        case result of
          Left err -> error $ show err
          Right _ -> return ()

spec2 :: Spec
spec2 = do
  businessLogicSpec
  
spec :: Spec
spec = do
    describe "arithmetic" $ do
      it "five == five" $ do -- \port -> do
        (pure 5) `shouldReturn` 5
  
main :: IO ()
main = hspec $ do
  spec
  businessLogicSpec
