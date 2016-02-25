
-- | Module containing everything you need to build 'Server's.
module Serv.Wai.Prelude (

    module Serv.Api
  , module Serv.Wai
  , module Serv.Wai.Response
  , module Serv.Wai.Rec
  , (&)
  , module Network.HTTP.Kinder

) where

import           Data.Function       ((&))
import           Network.HTTP.Kinder
import           Serv.Api
import           Serv.Wai
import           Serv.Wai.Rec
import           Serv.Wai.Response
