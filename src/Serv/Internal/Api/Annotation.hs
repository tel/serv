
-- | Classes and methods for ascribing annotation to API endpoints.
module Serv.Internal.Api.Annotation where

import Data.Text (Text)
import Data.Proxy

-- | A type is annotated if it has user-facing documentation. Types may
-- also be intentionally annotated with no user-facing documentation.
class Annotated a where
  annotationText :: Proxy a -> Maybe Text

instance Annotated () where
  annotationText _ = Nothing
