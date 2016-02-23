
module Serv.Internal.Server.Error where

import Data.Set (Set)
import           Serv.Internal.Verb

-- | Errors which arise during the "handling" portion of dealing with a response.
data RoutingError
  = NotFound
  | BadRequest (Maybe String)
  | UnsupportedMediaType
  | MethodNotAllowed (Set Verb)

-- | An ignorable error is one which backtracks the routing search
-- instead of forcing a response.
ignorable :: RoutingError -> Bool
ignorable NotFound = True
ignorable _ = False
