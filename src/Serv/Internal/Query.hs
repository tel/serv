{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.Query where

import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.String
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)
import qualified Network.HTTP.Types       as HTTP
import           Serv.Internal.Pair
import           Serv.Internal.RawText
import           Serv.Internal.Rec
import           Serv.Internal.TypeLevel

-- Classes
-- ----------------------------------------------------------------------------

class KnownSymbol s => QueryEncode s a where
  queryEncode :: Sing s -> a -> Maybe Text

class KnownSymbol s => QueryDecode s a where
  queryDecode :: Sing s -> Maybe Text -> Either String a

-- Analysis
-- ----------------------------------------------------------------------------

type AllEncoded rs = AllC (UncurrySym1 (TyCon2 QueryEncode)) rs

queryPair :: QueryEncode n a => Sing n -> a -> HTTP.QueryItem
queryPair s a =
  ( fromString (withKnownSymbol s (symbolVal s))
  , encodeUtf8 <$> queryEncode s a
  )

firstName :: SingI name => Rec (name ::: ty ': rs) -> Sing name
firstName _ = sing

encodeQueries :: AllEncoded query => Rec query -> HTTP.Query
encodeQueries rec =
  case rec of
    Nil -> []
    Cons val rest ->
      queryPair (firstName rec) val : encodeQueries rest


-- Instances
-- ----------------------------------------------------------------------------

instance (KnownSymbol s, SingI s) => QueryEncode s RawText where
  queryEncode _ (RawText t) = Just t

instance (KnownSymbol s, SingI s) => QueryDecode s RawText where
  queryDecode _ Nothing = Left "expected query value"
  queryDecode _ (Just t) = Right (RawText t)
