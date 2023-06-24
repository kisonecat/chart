{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes            #-}

module Course (API, server) where

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Server

import Authentication (AuthenticatedUser)
import qualified Database.Redis as R
                      
import Data.Aeson
import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8 )
import Data.Bool (bool)

import qualified Data.Text as Text
import Data.String.Conversions (cs)
import qualified Text.Email.Validate as Email
  
import Data.Typeable

import Data.ByteString.Char8 (pack)

import AppM ( AppM, AppCtx, getConfiguration, getPool )
import Configuration ( getHostname )

import Control.Monad.Except (liftEither, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader  (ReaderT, ask, asks)
import Control.Monad.Trans.Class
import Hashcash (Hashcash)
import Data.Pool (withResource)

data CourseIdentifier = CourseIdentifier { courseName :: !Text
                                         , courseDomain :: !(Maybe Text)
                                         }
                        deriving Show

instance FromHttpApiData CourseIdentifier where
  parseUrlPiece :: Text -> Either Text CourseIdentifier
  parseUrlPiece text = case Email.emailAddress (cs text) of
      Just address -> Right $ CourseIdentifier
                            (cs $ Email.localPart address)
                            (Just $ cs $ Email.domainPart address)
      Nothing -> Right $ CourseIdentifier text Nothing

instance ToHttpApiData CourseIdentifier where
  toUrlPiece :: CourseIdentifier -> Text
  toUrlPiece (CourseIdentifier name (Just domain)) =
    Text.concat [ name, "@", domain ]
  toUrlPiece (CourseIdentifier name Nothing) = name

data Course = Course { courseIdentifier :: CourseIdentifier
                     , courseDescription :: !(Maybe Text) 
                     }
              deriving Show

instance FromJSON Course where
    parseJSON (Object v) = do
      id <- v .: "id"
      desc <- v .:? "description" 
      let address = Email.emailAddress $ encodeUtf8 id 
      let name = maybe "" Email.localPart address
      let domain = maybe "" Email.domainPart address
      pure $ Course { courseIdentifier = CourseIdentifier (cs name) (Just $ cs domain)
                    , courseDescription = desc
                    }

    parseJSON _ = pure $ Course { courseIdentifier = CourseIdentifier "" Nothing
                                , courseDescription = Nothing
                                } 

instance ToJSON Course where
    toJSON :: Course -> Value
    toJSON (Course identifier description) = object fields
      where
        consMay attr = maybe id ((:) . (attr .=))
        conss = consMay "description" description
        fields = conss [ "id" .= toUrlPiece identifier ]

type ReadAPI = Hashcash :> Get '[JSON] Course
  
readCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> AppM Course

readCourse (Authenticated au) cid@(CourseIdentifier name _) = do
  let key = pack $ "course:" ++ cs name
  pool <- asks getPool
  withResource pool $ \conn -> do
    d <- liftIO $ R.runRedis conn $ R.hget key $ pack "description"
    case d of
      Left err -> throwError err500 { errBody = cs $ show err } 
      Right Nothing -> throwError err404 { errBody = "Course not found" }
      Right (Just description) -> pure $ Course cid $ Just $ cs description
readCourse _ _ = throwError err401

type CreateAPI = ReqBody '[JSON] Course :> Post '[JSON] Course
  
createCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> Course -> AppM Course
-- createCourse (Authenticated au) u = pure $ Course (CourseIdentifier "" Nothing) Nothing
createCourse _ _ _ = throwError err401

type UpdateAPI = ReqBody '[JSON] Course :> (Put '[JSON] Course :<|> Patch '[JSON] Course)

putCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> Course -> AppM Course
putCourse _ _ _ = throwError err401 

type DeleteAPI = Delete '[JSON] ()
    
deleteCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> AppM ()
deleteCourse (Authenticated au) u = pure ()
deleteCourse _ _ = throwError err401 

type API = SAS.Auth '[SA.JWT] AuthenticatedUser :> "courses" :>
           Capture "course" CourseIdentifier :> 
           ( CreateAPI :<|> ReadAPI :<|> UpdateAPI :<|> DeleteAPI )

canonicalizeCourseId :: CourseIdentifier -> AppM CourseIdentifier
canonicalizeCourseId (CourseIdentifier name Nothing) = do
  config <- asks getConfiguration
  let domain :: Text
      domain = cs $ getHostname config 
  pure $ CourseIdentifier name $ Just domain 
canonicalizeCourseId cid = pure cid

isCourseHomedHere :: CourseIdentifier -> AppM Bool
isCourseHomedHere cid@(CourseIdentifier name Nothing) = do
  cid' <- canonicalizeCourseId cid
  isCourseHomedHere cid'
isCourseHomedHere (CourseIdentifier name (Just domain)) = do
  config <- asks getConfiguration
  let domain' :: Text
      domain' = cs $ getHostname config 
  pure $ domain == domain' 

ensureCourseHomedHere :: CourseIdentifier -> AppM CourseIdentifier
ensureCourseHomedHere cid = do
  cid' <- canonicalizeCourseId cid
  b <- isCourseHomedHere cid'
  if b
    then pure cid' 
    else throwError err404 { errBody = "Course not in this domain" }

updateCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> ServerT UpdateAPI AppM 
updateCourse au c cc = do
  f putCourse au c cc :<|> f putCourse au c cc
  where f g a c cc = do
          c' <- ensureCourseHomedHere c 
          g a c' cc

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API AppM
server _ _ au c = do
         createCourse au c :<|> readCourse au c :<|> updateCourse au c :<|> deleteCourse au c

  --where f g a c c2 = do
 --         c' <- ensureCourseHomedHere c 
  --        g a c' c2
 --where f x = x
  --f2 createCourse au cid' :<|> f readCourse au cid' :<|> updateCourse au cid' c :<|> f deleteCourse au cid'
  --      f2 g a c c2 = do
  --        c' <- ensureCourseHomedHere c 
  --        g a c' c2
