{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MockM (MockM(..)
             , MockDatabase
             ) where

import qualified Database.Redis as R
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVarIO, readTVar, writeTVar)
import           Control.Monad.STM           (atomically)

import Data.ByteString
import qualified Data.Map as Map
import Data.Map (Map)

import AppM (AppCtx(..), MonadDB(..), HasConfiguration(..), HasSymmetricJWK(..), Key, Field)
import Network.Wai.Handler.Warp
import Servant.Server
import Crypto.JOSE

data MockValue =
  RedisValue ByteString |
  RedisSortedSet (Map Field Double) |
  RedisHash (Map Field ByteString)
 deriving (Show)

type MockDatabase = Map Key MockValue

newtype MockM a = MockM { runMock :: ReaderT (AppCtx, TVar MockDatabase) Handler a }
   deriving (Functor, Applicative, MonadReader (AppCtx, TVar MockDatabase), Monad, MonadIO, MonadError ServerError)
   -- also MonadTime, MonadLog?

instance HasConfiguration (AppCtx, a) where
  getConfiguration = _getConfiguration . fst       
  
instance HasSymmetricJWK (AppCtx, a) where
  getSymmetricJWK = _getSymmetricJWK . fst       

instance MonadRandom MockM where
    getRandomBytes = liftIO . getRandomBytes
    
instance MonadDB MockM where
  hset key field value = do
    p <- asks snd
    liftIO $ atomically $ readTVar p >>= writeTVar p . Map.alter f key
    pure $ Right 1
      where f :: Maybe MockValue -> Maybe MockValue
            f Nothing  = Just $ RedisHash $ Map.singleton field value
            f (Just (RedisHash h)) = Just $ RedisHash $ Map.insert field value h
    
  hget key field = do
    p <- asks snd
    x <- liftIO $ readTVarIO p    
    pure $ case Map.lookup key x of
      Nothing -> Right Nothing
      Just (RedisHash h) -> case Map.lookup field h of
        Nothing -> Right Nothing
        Just b -> Right $ Just b
      Just _ -> Left $ R.Error "wrong type"
    
  rget key = MockM $ do
    p <- asks snd
    x <- liftIO $ readTVarIO p    
    pure $ case Map.lookup key x of
      Nothing -> Right Nothing
      Just (RedisValue b) -> Right $ Just b
      Just _ -> Left $ R.Error "wrong type"
     
  rset key value = do
    p <- asks snd
    liftIO $ atomically $ readTVar p >>= writeTVar p . (Map.insert key $ RedisValue value)
    pure $ Right R.Ok
  
  zadd key double field = do
    p <- asks snd
    liftIO $ atomically $ readTVar p >>= writeTVar p . (Map.alter f key)
    pure $ Right 1
      where f :: Maybe MockValue -> Maybe MockValue
            f Nothing  = Just $ RedisSortedSet $ Map.singleton field double
            f (Just (RedisSortedSet h)) = Just $ RedisSortedSet $ Map.insert field double h
              
  zscore key field = do
    p <- asks snd
    x <- liftIO $ readTVarIO p    
    pure $ case Map.lookup key x of
      Nothing -> Right Nothing
      Just (RedisSortedSet h) -> case Map.lookup field h of
        Nothing -> Right Nothing
        Just d -> Right $ Just d
      Just _ -> Left $ R.Error "wrong type"


-- https://github.com/haskell-servant/servant/issues/458

