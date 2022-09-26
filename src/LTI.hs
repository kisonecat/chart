{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LTI where

import Servant
import Servant.API
import Web.LTI13

type API = "lti" :> ("launch" :> Get '[JSON] () :<|> "auth" :> Get '[JSON] ())

