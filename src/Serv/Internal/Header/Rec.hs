{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.Header.Rec where

import           Data.Proxy
import           Data.Text.Encoding                 (encodeUtf8)
import qualified Network.HTTP.Types                 as HTTP
import           Serv.Internal.Header.Name
import           Serv.Internal.Header.Serialization
import           Serv.Internal.Pair

-- | An HList collecting heterogenous types matched up to an HTTP
-- header providing semantics.
data Rec rs where
  Nil :: Rec '[]
  Cons :: ty -> Rec rs -> Rec ( name '::: ty ': rs )

-- | Append a new header value on to a header record
(-:) :: Proxy name -> ty -> Rec rs -> Rec (name '::: ty ': rs)
(-:) _ = Cons



-- | Given a record of headers, encode them into a list of header pairs.
class ReflectHeaders headers where
  reflectHeaders :: Rec headers -> [HTTP.Header]

instance ReflectHeaders '[] where
  reflectHeaders Nil = []

instance
  (ReflectName name, HeaderEncode name ty, ReflectHeaders headers) =>
    ReflectHeaders ( name '::: ty ': headers )
  where
    reflectHeaders (Cons val headers) =
      -- NOTE: Utf8 encoding is somewhat significantly too lax
        (reflectName proxy, encodeUtf8 (headerEncode proxy val)) : reflectHeaders headers
      where proxy = Proxy :: Proxy name
