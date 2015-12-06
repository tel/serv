
module Serv.Internal.Server.Error where

import Serv.Internal.Response

-- | Errors which arise during the "handling" portion of dealing with a response.
data RoutingError
  = NotFound
  | BadRequest (Maybe String)
  | UnsupportedMediaType
  | MethodNotAllowed [Verb]

-- | An ignorable error is one which backtracks the routing search
-- instead of forcing a response.
ignorable :: RoutingError -> Bool
ignorable NotFound = True
ignorable _ = False
