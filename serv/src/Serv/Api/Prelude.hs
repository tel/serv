
-- | Module containing everything you need to define an 'Api' type. Import
-- this unqualified for easy 'Api' definitions.
--
-- Exports the instance of 'SingI' for all 'Typeable' types. This will
-- cause issues for @CustomStar@-style 'SingI' instances, but is nearly
-- required for Serv.
module Serv.Api.Prelude (

    module Serv.Api
  , module Network.HTTP.Kinder
  , SingI (..)

) where

import           Data.Singletons
import           Data.Singletons.TypeRepTYPE ()
import           Network.HTTP.Kinder
import           Serv.Api
