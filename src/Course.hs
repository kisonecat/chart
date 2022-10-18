{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Course (API, server) where

import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Server

import Authentication (AuthenticatedUser)
                      
import Data.Aeson
import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8 )

import qualified Data.Text as Text
import Data.String.Conversions (cs)
import qualified Text.Email.Validate as Email

import AppM ( AppM )

import Control.Monad.Except (liftEither, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)

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
        
type ReadAPI = Get '[JSON] Course
    
readCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> AppM Course
readCourse (Authenticated au) courseId = throwError err500
readCourse _ _ = throwError err401

type CreateAPI = ReqBody '[JSON] Course :> Post '[JSON] Course
  
createCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> AppM Course
--createCourse (Authenticated au) u = pure $ Course { courseName = ""
--                                              , courseDomain = ""
--                                               , courseDescription = Nothing
--                                               }
createCourse _ _ = throwError err401 { errBody = "" }

type UpdateAPI = ReqBody '[JSON] Course :> (Put '[JSON] Course :<|> Patch '[JSON] Course)

putCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> Course -> AppM Course
--createCourse (Authenticated au) u = pure $ Course { courseName = ""
--                                              , courseDomain = ""
--                                               , courseDescription = Nothing
--                                               }
putCourse _ _ _ = throwError err401 

updateCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> ServerT UpdateAPI AppM 
updateCourse au c cc = putCourse au c cc :<|> putCourse au c cc

type DeleteAPI = Delete '[JSON] ()
    
deleteCourse :: AuthResult AuthenticatedUser -> CourseIdentifier -> AppM ()
deleteCourse (Authenticated au) u = pure ()
deleteCourse _ _ = throwError err401 

type API = SAS.Auth '[SA.JWT] AuthenticatedUser :> "courses" :>
           Capture "course" CourseIdentifier :>
           ( CreateAPI :<|> ReadAPI :<|> UpdateAPI :<|> DeleteAPI )
           --( ReadAPI )

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT API AppM
server _ _ au u =
   createCourse au u :<|> 
   readCourse au u :<|> 
   updateCourse au u :<|> 
   deleteCourse au u
