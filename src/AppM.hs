{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module AppM (AppCtx(..), AppM) where

import qualified Database.Redis as R
import           Control.Monad.Trans.Reader  (ReaderT, ask, asks, runReaderT)

import qualified Crypto.JOSE.JWK as JOSE
import Crypto.JOSE
import Crypto.JWT (signClaims, SignedJWT)
import Crypto.JOSE.JWS (newJWSHeader)

import Servant.Server
import Data.Pool (Pool)

import Configuration
    ( Configuration(getHostname) )

import Network.Wai.Handler.Warp

data AppCtx = AppCtx { getConfiguration :: Configuration
                     , getPool :: Pool R.Connection
                     , getJWK :: JWK
                     , getSymmetricJWK :: JWK
                     }
                       
type AppM = ReaderT AppCtx Handler
