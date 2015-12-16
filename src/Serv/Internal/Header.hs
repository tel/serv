{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Serv.Internal.Header (

  HeaderName (..),
  reflectName,
  ReflectName,
  ReflectHeaders (..),
  ReflectHeaderNames (..),
  HeaderEncode (..),
  headerEncodeRaw,
  HeaderDecode (..),
  headerDecodeRaw

  ) where

import           Data.Proxy
import           Data.Text.Encoding                 (encodeUtf8)
import qualified Network.HTTP.Types                 as HTTP
import           Serv.Internal.Header.Name
import           Serv.Internal.Header.Serialization
import           Serv.Internal.Pair
import           Serv.Internal.Rec

-- | Given a header set description, reflect out all of the names
class ReflectHeaderNames headers where
  reflectHeaderNames :: Proxy headers -> [HTTP.HeaderName]

instance ReflectHeaderNames '[] where
  reflectHeaderNames _ = []

instance
  (ReflectName name, ReflectHeaderNames hdrs) =>
  ReflectHeaderNames (name '::: ty ': hdrs)
  where
    reflectHeaderNames _ =
      reflectName (Proxy :: Proxy name)
      : reflectHeaderNames (Proxy :: Proxy hdrs)

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
