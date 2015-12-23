
module Serv.Internal.Cors where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe                         (catMaybes)
import           Data.Monoid
import           Data.Proxy
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import           Data.Time
import qualified Network.HTTP.Types                 as HTTP
import qualified Serv.Internal.Header               as Header
import qualified Serv.Internal.Header.Serialization as Hs
import           Serv.Internal.Server.Config
import           Serv.Internal.Verb                 (Verb)

-- | A 'CorsPolicy' interprets the request's provided Origin and the
-- current routing context to determine how to expose resources to the
-- requestor.
type Policy = Config -> Context -> AccessSet

-- | Class describing types which describe CORS 'Policy's.
class CorsPolicy m where
  corsPolicy :: proxy m -> Policy

-- | The 'PermitAll' policy produces the `permitAll` `Policy`.
data PermitAll

instance CorsPolicy PermitAll where
  corsPolicy _ = permitAll

data IncludeMethods
  = IncludeMethods | Don'tIncludeMethods
  deriving (Eq, Ord, Show, Read)

headerSet :: IncludeMethods -> Context -> AccessSet -> [HTTP.Header]
headerSet includeMethods ctx access
  | not (originAllowed access) = []
  | otherwise =
      catMaybes
      [ Hs.headerPair Header.SAccessControlMaxAge (maxAge access)
      , do guard (includeMethods == IncludeMethods)
           Hs.headerPair Header.SAccessControlAllowMethods (methodsAllowed access)
      , Hs.headerPair Header.SAccessControlAllowOrigin (origin ctx)
      , Hs.headerPair Header.SAccessControlExposeHeaders (headersExposed access)
      , Hs.headerPair Header.SAccessControlAllowHeaders (headersAllowed access)
      , Hs.headerPair Header.SAccessControlAllowCredentials (credentialsAllowed access)
      ]

-- | The 'CorsContext' provides data from which we can make choices about
-- how to respond to CORS requests.
data Context
  = Context
   { origin           :: Text
   , headersExpected  :: Set (Header.HeaderType Text)
   , headersReturned  :: Set (Header.HeaderType Text)
   , methodsAvailable :: Set Verb
   }

mergeContext :: Context -> Context -> Context
mergeContext a b =
  Context
  { origin = origin a
  , headersExpected = headersExpected a <> headersExpected b
  , headersReturned = headersReturned a <> headersReturned b
  , methodsAvailable = methodsAvailable a <> methodsAvailable b
  }

-- | Descrbes what parts of the response should be made available
-- cross-origin. The Monoid product on 'AccessSet's permits all accesses of
-- either of the constituents.
data AccessSet =
  AccessSet
  { originAllowed      :: Bool
  , headersExposed     :: Set (Header.HeaderType Text)
  , credentialsAllowed :: Bool
  , methodsAllowed     :: Set Verb
  , headersAllowed     :: Set (Header.HeaderType Text)
  , maxAge             :: Maybe NominalDiffTime
  }

-- | The empty access set disallows all CORS access while the product @l <>
-- r@ provides access to a particular part of the response if either @l@ or
-- @r@ does.
instance Monoid AccessSet where
  mempty =
    AccessSet
    { originAllowed = False
    , headersExposed = mempty
    , credentialsAllowed = False
    , methodsAllowed = mempty
    , headersAllowed = mempty
    , maxAge = Nothing
    }
  mappend a b =
    AccessSet
    { originAllowed = originAllowed a || originAllowed b
    , headersExposed = headersExposed a <> headersExposed b
    , credentialsAllowed = credentialsAllowed a || credentialsAllowed b
    , methodsAllowed = methodsAllowed a <> methodsAllowed b
    , headersAllowed = headersAllowed a <> headersAllowed b
    , maxAge = liftA2 max (maxAge a) (maxAge b)
    }


-- | The most permissive CORS 'Policy' possible. Differs from Wildcard in
-- that it allows credentials. Max age is not provided (so no caching)
permitAll :: Policy
permitAll _config ctx =
  AccessSet
  { originAllowed  = True
  , headersExposed = headersReturned ctx
  , headersAllowed = headersExpected ctx
  , credentialsAllowed = True
  , methodsAllowed = methodsAvailable ctx
  , maxAge = Nothing
  }

-- | Effects a wildcard policy which provides maximal cross-origin access
-- to all request origins. This disallows credentials use, however.
wildcard :: Policy
wildcard config ctx =
  (permitAll config ctx)
  { credentialsAllowed = False }

-- | Provides access to all origins which pass a predicate
predicateWhitelist :: (Text -> Bool) -> Policy
predicateWhitelist originOk config ctx =
  (permitAll config ctx)
  { originAllowed = originOk (origin ctx) }
