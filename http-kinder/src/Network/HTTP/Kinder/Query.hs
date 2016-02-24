{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | A request can be thought of as having a query component which is
-- a mapping from a set of query keys (which are just strings) to values of
-- @'QueryKeyState' 'Text'@.
--
-- This module provides tools for extracting information from this mapping
-- or constructing them.
module Network.HTTP.Kinder.Query (

  -- * Classes for encoding and decoding
    QueryEncode (..)
  , QueryDecode (..)

  -- ** Listing constraints to type-level lists

  , AllQueryEncodes
  , AllQueryDecodes

  -- ** Types for encoding/decoding request queries
  , QueryKeyState (..)
  , Flag (..)

  -- * Extra serialization utilities
  , queryEncodePair

) where

import           Control.Monad
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.String
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           GHC.Exts
import           Network.HTTP.Kinder.Common
import qualified Network.HTTP.Types         as HTTP
import           Network.HTTP.Types.URI     (queryToQueryText)

-- Data Types
-- ----------------------------------------------------------------------------

-- | 'Flag' provides semantics for query parameters which may merely exist
-- without actually storing a value---"flag" semantics.
data Flag = Here | NotHere

-- | 'QueryKeyState' describes the state of a given key within a query-map.
-- The key may be complete absent, it may exist without a value, or it may
-- exist with some value at a given type.
data QueryKeyState a
  = QueryKeyPresent
  | QueryKeyValued a
  | QueryKeyAbsent
  deriving ( Eq, Ord, Show, Read, Functor )

instance Applicative QueryKeyState where
  pure = QueryKeyValued
  (<*>) = ap

-- | Monad instance equivalent to @Either Bool@
instance Monad QueryKeyState where
  return = pure
  m >>= f =
    case m of
      QueryKeyValued a -> f a
      QueryKeyPresent -> QueryKeyPresent
      QueryKeyAbsent -> QueryKeyAbsent

-- Classes
-- ----------------------------------------------------------------------------

-- | Determines a representation of a query value for a given query key.
class QueryEncode (s :: Symbol) a where
  queryEncode :: sing s -> a -> QueryKeyState Text


-- | For a given concrete type @a@, a list of pairs @ts@ satisfies
-- @'AllQueryEncode' a ts@ if each @(n, a)@ in @ts@ has @'QueryEncode'
-- n a@.
type family AllQueryEncodes hs :: Constraint where
  AllQueryEncodes '[] = ()
  AllQueryEncodes ( '(s, a) ': hs ) = (QueryEncode s a, AllQueryEncodes hs)


-- | Attempts to parse a representation of a query value at a given query
-- key.
class QueryDecode (s :: Symbol) a where
  queryDecode :: sing s -> QueryKeyState Text -> Either String a

-- | For a given concrete type @a@, a list of pairs @ts@ satisfies
-- @'AllQueryDecode' a ts@ if each @(n, a)@ in @ts@ has @'QueryDecode'
-- n a@.
type family AllQueryDecodes hs :: Constraint where
  AllQueryDecodes '[] = ()
  AllQueryDecodes ( '(s, a) ': hs ) = (QueryDecode s a, AllQueryDecodes hs)


-- | Produces a pair of @(name, representation)@ from a given query encoding.
queryEncodePair :: (KnownSymbol n, QueryEncode n a) => Sing n -> a -> Maybe (Text, Maybe Text)
queryEncodePair s a =
  case queryEncode s a of
    QueryKeyAbsent -> Nothing
    QueryKeyPresent -> Just (name, Nothing)
    QueryKeyValued v -> Just (name, Just v)
  where
    name = fromString (withKnownSymbol s (symbolVal s))

-- Instances
-- ----------------------------------------------------------------------------

instance QueryEncode s a => QueryEncode s (QueryKeyState a) where
  queryEncode p v = v >>= queryEncode p

instance QueryEncode s Flag where
  queryEncode _ Here = QueryKeyPresent
  queryEncode _ NotHere = QueryKeyAbsent

instance QueryEncode s (Raw Text) where
  queryEncode _ (Raw t) = QueryKeyValued t

-- Handles the common case of an optional value. In other words, this will
-- treat a query parameter which was provided without a value as having
-- actually provided an empty value @""@.
instance QueryEncode s a => QueryEncode s (Maybe a) where
  queryEncode _ Nothing = QueryKeyAbsent
  queryEncode p (Just a) =
    case queryEncode p a of
      QueryKeyAbsent -> QueryKeyAbsent
      QueryKeyPresent -> QueryKeyValued ""
      QueryKeyValued txt -> QueryKeyValued txt

instance QueryDecode s a => QueryDecode s (QueryKeyState a) where
  queryDecode p v =
    case v of
      QueryKeyAbsent -> Right QueryKeyAbsent
      QueryKeyPresent -> Right QueryKeyPresent
      QueryKeyValued _ ->
        case queryDecode p v of
          Left err -> Left err
          Right val -> Right (QueryKeyValued val)

-- | This instance act "strictly" in that if the key is present but given
-- a value then it will fail to parse for this type. To be lenient use
-- 'QueryKeyState' directly.
instance QueryDecode s Flag where
  queryDecode _ QueryKeyAbsent = Right NotHere
  queryDecode _ QueryKeyPresent = Right Here
  queryDecode _ QueryKeyValued {} = Left "expected a flag query param, found a value"

instance QueryDecode s (Raw Text) where
  queryDecode _ QueryKeyAbsent = Left "expected query key"
  queryDecode _ QueryKeyPresent = Left "expected query value"
  queryDecode _ (QueryKeyValued t) = Right (Raw t)

instance QueryDecode s a => QueryDecode s (Maybe a) where
  queryDecode _ QueryKeyAbsent = Right Nothing
  queryDecode p QueryKeyPresent = queryDecode p (QueryKeyValued "")
  queryDecode p (QueryKeyValued t) = fmap Just (queryDecode p (QueryKeyValued t))
