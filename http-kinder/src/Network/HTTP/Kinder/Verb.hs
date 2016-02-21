{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines types and kinds for working with type and value level HTTP
-- verbs.
module Network.HTTP.Kinder.Verb where

import qualified Data.ByteString      as S
import qualified Data.CaseInsensitive as CI
import           Data.Singletons.Prelude
import           Data.String

-- | A data type representing HTTP verbs. Much more importantly, with
-- @DataKinds@ enabled this becomes a kind describing types, one for each
-- such verb.
--
-- Use 'Verb' at both the kind and type levels---it works equally well at
-- both unlike, e.g., 'HeaderName'. Use methods of 'SingKind' to convert
-- between 'Verb' values and 'Verb' 'Sing's
--
-- Note: TRACE is intentionally omitted because (a) it's very low value and
-- (b) it opens a potential security hole via Cross-Site-Tracing.
data Verb
  = DELETE
  | GET
  | HEAD
  | OPTIONS
  | PATCH
  | POST
  | PUT
    deriving ( Eq, Ord, Show, Read )

type DELETE = 'DELETE
type GET = 'GET
type HEAD = 'HEAD
type OPTIONS = 'OPTIONS
type PATCH = 'PATCH
type POST = 'POST
type PUT = 'PUT

data instance Sing (v :: Verb)
  = v ~ DELETE => SDELETE
  | v ~ GET => SGET
  | v ~ HEAD => SHEAD
  | v ~ OPTIONS => SOPTIONS
  | v ~ PATCH => SPATCH
  | v ~ POST => SPOST
  | v ~ PUT => SPUT

instance SingI 'DELETE where sing = SDELETE
instance SingI 'GET where sing = SGET
instance SingI 'HEAD where sing = SHEAD
instance SingI 'OPTIONS where sing = SOPTIONS
instance SingI 'PATCH where sing = SPATCH
instance SingI 'POST where sing = SPOST
instance SingI 'PUT where sing = SPUT

instance SingKind ('KProxy :: KProxy Verb) where
  type DemoteRep ('KProxy :: KProxy Verb) = Verb
  fromSing s =
    case s of
      SDELETE -> DELETE
      SGET -> GET
      SHEAD -> HEAD
      SOPTIONS -> OPTIONS
      SPATCH -> PATCH
      SPOST -> POST
      SPUT -> PUT
  toSing v =
    case v of
      DELETE -> SomeSing SDELETE
      GET -> SomeSing SGET
      HEAD -> SomeSing SHEAD
      OPTIONS -> SomeSing SOPTIONS
      PATCH -> SomeSing SPATCH
      POST -> SomeSing SPOST
      PUT -> SomeSing SPUT

-- | Convert a 'Verb' to its string-like representation
verbName :: IsString t => Verb -> t
verbName v =
  case v of
    GET -> "GET"
    HEAD -> "HEAD"
    POST -> "POST"
    PUT -> "PUT"
    PATCH -> "PATCH"
    DELETE -> "DELETE"
    OPTIONS -> "OPTIONS"

-- | Attempt to parse a string-like representation of a 'Verb'.
parseVerb :: S.ByteString -> Maybe Verb
parseVerb s =
  case CI.mk s of
    "GET" -> Just GET
    "HEAD" -> Just HEAD
    "POST" -> Just POST
    "PUT" -> Just PUT
    "PATCH" -> Just PATCH
    "DELETE" -> Just DELETE
    "OPTIONS" -> Just OPTIONS
    _ -> Nothing
