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

module Serv.Internal.Query where

import           Control.Monad
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.String
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)
import           GHC.Exts
import qualified Network.HTTP.Types       as HTTP
import           Serv.Internal.Pair
import           Serv.Internal.RawText
import           Serv.Internal.Rec
import           Serv.Internal.TypeLevel

-- Data Types
-- ----------------------------------------------------------------------------

-- | 'Found' provides semantics for query parameters which may merely exist
-- without actually storing a value---"flag" semantics.
data Found = Found | NotFound

-- TODO: Produce a Monad instance for QueryKeyState so that we can layer
-- them. Provide the semantics at the level of that type so that they can
-- be consistently interpreted in instances definted atop it.

-- | 'QueryKeyState' describes the state of a given key within a query-map.
-- The key may be complete absent, it may exist without a value, or it may
-- exist with some value at a given type.
data QueryKeyState a
  = KeyPresent
  | KeyValued a
  | KeyAbsent
  deriving ( Eq, Ord, Show, Read, Functor )

instance Applicative QueryKeyState where
  pure = KeyValued
  (<*>) = ap

-- | Monad instance equivalent to @Either Bool@
instance Monad QueryKeyState where
  return = pure
  m >>= f =
    case m of
      KeyValued a -> f a
      KeyPresent -> KeyPresent
      KeyAbsent -> KeyAbsent

-- Classes
-- ----------------------------------------------------------------------------

-- | Query semantics are defined against the names of the query parameters
-- available.
class QueryEncode s a where
  queryEncode :: proxy s -> a -> QueryKeyState Text

class QueryDecode s a where
  queryDecode :: proxy s -> QueryKeyState Text -> Either String a

-- Analysis
-- ----------------------------------------------------------------------------

-- | Lifts the constraint on the pair type of a symbol and a type such that
-- the symbol is "known" and that the type has a query encoding at that
-- symbol to a list of such tuples.
type family AllCanEncode (rs :: [(Symbol, *)]) :: Constraint where
  AllCanEncode '[] = ()
  AllCanEncode ( '(n, ty) ': rs ) =
    (KnownSymbol n, QueryEncode n ty, AllCanEncode rs)

queryPair :: (KnownSymbol n, QueryEncode n a) => Sing n -> a -> Maybe HTTP.QueryItem
queryPair s a =
  case queryEncode s a of
    KeyAbsent -> Nothing
    KeyPresent -> Just (name, Nothing)
    KeyValued v -> Just (name, Just (encodeUtf8 v))
  where
    name = fromString (withKnownSymbol s (symbolVal s))

firstName :: SingI name => Rec (name ::: ty ': rs) -> Sing name
firstName _ = sing

encodeQueries :: AllCanEncode query => Rec query -> HTTP.Query
encodeQueries rec =
  case rec of
    Nil -> []
    Cons val rest ->
      case queryPair (firstName rec) val of
        Nothing -> encodeQueries rest
        Just newQuery -> newQuery : encodeQueries rest


-- Instances
-- ----------------------------------------------------------------------------

instance QueryEncode s a => QueryEncode s (QueryKeyState a) where
  queryEncode p v = v >>= queryEncode p

instance QueryEncode s Found where
  queryEncode _ Found = KeyPresent
  queryEncode _ NotFound = KeyAbsent

instance QueryEncode s RawText where
  queryEncode _ (RawText t) = KeyValued t

-- Handles the common case of an optional value. In other words, this will
-- treat a query parameter which was provided without a value as having
-- actually provided an empty value @""@.
instance QueryEncode s a => QueryEncode s (Maybe a) where
  queryEncode p Nothing = KeyAbsent
  queryEncode p (Just a) =
    case queryEncode p a of
      KeyAbsent -> KeyAbsent
      KeyPresent -> KeyValued ""
      KeyValued txt -> KeyValued txt

instance QueryDecode s a => QueryDecode s (QueryKeyState a) where
  queryDecode p v =
    case v of
      KeyAbsent -> Right KeyAbsent
      KeyPresent -> Right KeyPresent
      KeyValued _ ->
        case queryDecode p v of
          Left err -> Left err
          Right val -> Right (KeyValued val)

-- | This instance act "strictly" in that if the key is present but given
-- a value then it will fail to parse for this type. To be lenient use
-- 'QueryKeyState' directly.
instance QueryDecode s Found where
  queryDecode _ KeyAbsent = Right NotFound
  queryDecode _ KeyPresent = Right Found
  queryDecode _ KeyValued {} = Left "expected a flag query param, found a value"

instance (KnownSymbol s, SingI s) => QueryDecode s RawText where
  queryDecode _ KeyAbsent = Left "expected query key"
  queryDecode _ KeyPresent = Left "expected query value"
  queryDecode _ (KeyValued t) = Right (RawText t)

instance QueryDecode s a => QueryDecode s (Maybe a) where
  queryDecode _ KeyAbsent = Right Nothing
  queryDecode p KeyPresent = queryDecode p (KeyValued "")
  queryDecode p (KeyValued t) = fmap Just (queryDecode p (KeyValued t))
