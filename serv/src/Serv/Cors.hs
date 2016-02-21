
module Serv.Cors (

    Policy
  , Context (..)
  , AccessSet (..)

  , CorsPolicy (..)
  , PermitAll

  , permitAll
  , wildcard
  , predicateWhitelist

  ) where

import Serv.Internal.Cors
