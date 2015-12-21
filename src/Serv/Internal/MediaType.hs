{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.MediaType where

import qualified Data.ByteString             as S
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeRepStar
import           Network.HTTP.Media          (MediaType)
import qualified Network.HTTP.Media          as Media
import           Serv.Internal.TypeLevel

-- Classes
-- ----------------------------------------------------------------------------

class HasMediaType cty where
  mediaType :: Sing cty -> MediaType

class HasMediaType cty => MimeEncode a cty where
  mimeEncode :: Sing cty -> a -> S.ByteString

class HasMediaType cty => MimeDecode a cty where
  mimeDecode :: Sing cty -> S.ByteString -> Either String a

-- Analysis
-- ----------------------------------------------------------------------------

type AllEncoded a rs = AllC (TyCon1 (MimeEncode a)) rs
type AllDecoded a rs = AllC (TyCon1 (MimeDecode a)) rs

encoders :: AllEncoded a rs => Sing rs -> [(MediaType, a -> S.ByteString)]
encoders s =
  case s of
    SNil -> []
    SCons r rs -> (mediaType r, mimeEncode r) : encoders rs

decoders :: AllDecoded a rs => Sing rs -> [(MediaType, S.ByteString -> Either String a)]
decoders s =
  case s of
    SNil -> []
    SCons r rs -> (mediaType r, mimeDecode r) : decoders rs

negotiateContent
  :: AllEncoded a ctypes
  => Sing ctypes -> [Media.Quality MediaType]
  -> a -> Maybe (MediaType, S.ByteString)
negotiateContent sing acceptable value =
  fmap
  (\(mt, encoder) -> (mt, encoder value))
  (Media.mapQuality (map dblFst (encoders sing)) acceptable)
  where
    dblFst (mt, enc) = (mt, (mt, enc))

-- | Similar to 'negotiateContent' but will always attempt to provide
-- the first content type the server offers if nothing is acceptable.
-- Still fails when no content types are offered (what's going on?)
negotiateContentAlways
  :: AllEncoded a ctypes =>
     Sing ctypes -> [Media.Quality MediaType] -> a -> Maybe (MediaType, S.ByteString)
negotiateContentAlways sing acceptable value =
  case negotiateContent sing acceptable value of
    Nothing -> case encoders sing of
      [] -> Nothing
      ((mt, encoder) : _) -> Just (mt, encoder value)
    Just result -> Just result

tryDecode
  :: AllDecoded a ctypes =>
     Sing ctypes -> S.ByteString -> S.ByteString -> Maybe (Either String a)
tryDecode sing mt body =
  fmap
  (\decoder -> decoder body)
  (Media.mapContentMedia (decoders sing) mt)
