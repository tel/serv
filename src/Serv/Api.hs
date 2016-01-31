
module Serv.Api (

    Api (..)
  , Endpoint, OneOf, Raw, (:>)

  , Path (..)
  , Const, HeaderAs, Seg, Header, Wildcard, Cors

  , Handler (..)
  , Method, CaptureBody, CaptureHeaders, CaptureQuery
  , Output (..)
  , Respond

  , Body (..)
  , HasBody, Empty

  , Verb (..)
  , DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT

  , (:::)

  , Sing, SingI (..)

  ) where

import           Data.Singletons
import           Serv.Internal.Api
import           Serv.Internal.Pair
import           Serv.Internal.Verb
