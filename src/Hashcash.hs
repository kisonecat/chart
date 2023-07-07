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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hashcash (Hashcash) where

import Servant
import Servant.Server
import Servant.Server.Internal
import Data.Typeable

import Network.Wai (Request, requestHeaders, rawPathInfo)
import Control.Monad (unless)

import Data.Time.Clock.POSIX
import           Control.Monad.IO.Class      (liftIO)

import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)

import JsonWorkProof

paymentRequired = err402 { errBody = "Invalid or missing JSON work proof" }

-- I am very thankful for
-- https://www.williamyaoh.com/posts/2023-02-28-writing-servant-combinators.html
-- which explained how to build such server combinators

data Hashcash 
  deriving Typeable

instance (HasServer api context) => HasServer (Hashcash :> api) context where

  type ServerT (Hashcash :> api) m = ServerT api m

  hoistServerWithContext :: Proxy (Hashcash :> api) ->
                            Proxy context ->
                            (forall x. m x -> n x) ->
                            ServerT (Hashcash :> api) m ->
                            ServerT (Hashcash :> api) n
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api) 
  
  route :: Proxy (Hashcash :> api) -> Context context
          -> Delayed env (Server (Hashcash :> api))
          -> Router env

  route _ ctx server =
    let serverDummyArg = fmap const server
    in route (Proxy @api) ctx $
         addHeaderCheck
           serverDummyArg
           (withRequest $ \req -> do
               let maybeToken = lookup "JSON-Work-Proof" (requestHeaders req)
               let eitherJwp = maybe (Left "Missing JSON-Work-Proof header") decodeJWP maybeToken
               case eitherJwp of
                 Left e -> delayedFailFatal $ err402 { errBody = fromStrict . encodeUtf8 . pack $ e }
                 Right jwp -> do
                   let pi = rawPathInfo req
                   v <- liftIO $ verify jwp 10 (decodeUtf8 pi)
                   case v of
                     Left e -> delayedFailFatal $ err402 { errBody = fromStrict . encodeUtf8 . pack $ e }
                     Right () -> pure ()) 
