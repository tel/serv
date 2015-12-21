{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.MediaType where

import qualified Data.ByteString    as S
import           Data.Proxy
import           Network.HTTP.Media (MediaType)
import qualified Network.HTTP.Media as Media

-- Classes
-- ----------------------------------------------------------------------------

class HasMediaType ty where
  mediaType :: Proxy ty -> MediaType

class HasMediaType ty => MimeEncode ty val where
  mimeEncode :: Proxy ty -> val -> S.ByteString

class HasMediaType ty => MimeDecode ty val where
  mimeDecode :: Proxy ty -> S.ByteString -> Either String val

negotiateContent
  :: ReflectEncoders ctypes a =>
     Proxy ctypes -> [Media.Quality MediaType] -> a -> Maybe (MediaType, S.ByteString)
negotiateContent proxy acceptable value =
  fmap
  (\(mt, encoder) -> (mt, encoder value))
  (Media.mapQuality (map (\(mt, enc) -> (mt, (mt, enc))) $ reflectEncoders proxy) acceptable)

-- | Similar to 'negotiateContent' but will always attempt to provide
-- the first content type the server offers if nothing is acceptable.
-- Still fails when no content types are offered (what's going on?)
negotiateContentAlways
  :: ReflectEncoders ctypes a =>
     Proxy ctypes -> [Media.Quality MediaType] -> a -> Maybe (MediaType, S.ByteString)
negotiateContentAlways proxy acceptable value =
  case negotiateContent proxy acceptable value of
    Nothing -> case reflectEncoders proxy of
      [] -> Nothing
      ((mt, encoder) : _) -> Just (mt, encoder value)
    Just result -> Just result

class ReflectEncoders cts ty where
  reflectEncoders :: Proxy cts -> [(MediaType, ty -> S.ByteString)]

instance ReflectEncoders '[] ty where
  reflectEncoders Proxy = []

instance
  (ReflectEncoders cts ty, MimeEncode ct ty) =>
    ReflectEncoders (ct ': cts) ty
  where
    reflectEncoders Proxy =
      (mediaType pCt, mimeEncode pCt) : reflectEncoders pCts
      where
        pCt = Proxy :: Proxy ct
        pCts = Proxy :: Proxy cts


tryDecode
  :: ReflectDecoders ctypes a =>
     Proxy ctypes -> S.ByteString -> S.ByteString -> Maybe (Either String a)
tryDecode proxy mt body =
  fmap
  (\decoder -> decoder body)
  (Media.mapContentMedia (reflectDecoders proxy) mt)

class ReflectDecoders cts ty where
  reflectDecoders :: Proxy cts -> [(MediaType, S.ByteString -> Either String ty)]

instance ReflectDecoders '[] ty where
  reflectDecoders Proxy = []

instance
  (MimeDecode ct ty, ReflectDecoders cts ty) =>
    ReflectDecoders (ct ': cts) ty
  where
    reflectDecoders Proxy =
      (mediaType pCt, mimeDecode pCt) : reflectDecoders pCts
      where
        pCt = Proxy :: Proxy ct
        pCts = Proxy :: Proxy cts
