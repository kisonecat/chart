{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM (AppCtx(..)
            , AppM(..)
            , MonadDB(..)
            , HasConfiguration(..)
            , HasSymmetricJWK(..)
            ) where

import qualified Database.Redis as R
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except
import Control.Monad.Reader

import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE
import Crypto.JWT (signClaims, SignedJWT)
import Crypto.JOSE.JWS (newJWSHeader)

import Servant.Server
import Data.Pool (Pool, withResource)

import Configuration
    ( Configuration(getHostname) )

import Network.Wai.Handler.Warp

data AppCtx = AppCtx { _getConfiguration :: Configuration
                     , getPool :: Pool R.Connection
                     , getJWK :: JWK
                     , _getSymmetricJWK :: JWK
                     }
                       
--type AppM = ReaderT AppCtx Handler

class MonadDB m where
    withConnection :: (R.Connection -> m a) -> m a

newtype AppM a = AppM { runApp :: ReaderT AppCtx Handler a }
   deriving (Functor, Applicative, MonadReader AppCtx, Monad, MonadIO, MonadError ServerError)
   -- also MonadTime, MonadLog?

instance MonadDB AppM where
    withConnection f = AppM $ do
        pool <- asks getPool
        withResource pool $ \conn -> runApp (f conn)

instance MonadRandom AppM where
    getRandomBytes = liftIO . getRandomBytes

class HasConfiguration a where
  getConfiguration :: a -> Configuration

instance HasConfiguration AppCtx where
  getConfiguration = _getConfiguration       
  
class HasSymmetricJWK a where
  getSymmetricJWK :: a -> JWK

instance HasSymmetricJWK AppCtx where
  getSymmetricJWK = _getSymmetricJWK       
