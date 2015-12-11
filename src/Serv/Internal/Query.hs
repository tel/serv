{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.Query where

import           Data.Proxy
import           Data.String
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           GHC.TypeLits
import qualified Network.HTTP.Types    as HTTP
import           Serv.Internal.Pair
import           Serv.Internal.RawText
import           Serv.Internal.Rec

class QueryEncode (s :: Symbol) a where
  queryEncode :: Proxy s -> a -> Maybe Text

class QueryDecode (s :: Symbol) a where
  queryDecode :: Proxy s -> Maybe Text -> Either String a

instance QueryEncode s RawText where
  queryEncode _ (RawText t) = Just t

instance QueryDecode s RawText where
  queryDecode _ Nothing = Left "expected query value"
  queryDecode _ (Just t) = Right (RawText t)

-- | Given a record of headers, encode them into a list of header pairs.
class ReflectQuery query where
  reflectQuery :: Rec query -> HTTP.Query

instance ReflectQuery '[] where
  reflectQuery Nil = []

instance
  (KnownSymbol s, QueryEncode s a, ReflectQuery query) =>
    ReflectQuery ( s '::: a ': query )
  where
    reflectQuery (Cons val rest) = (name, value) : reflectQuery rest
      where
        proxy = Proxy :: Proxy s
        name = fromString (symbolVal proxy)
        value = encodeUtf8 <$> queryEncode proxy val
