{-# LANGUAGE FlexibleInstances #-}

module Network.HTTP.Kinder.URI (

  -- * Classes for encoding and decoding
    URIEncode (..)
  , URIDecode (..)

  -- * Extra serialization utilities
  , uriDecodeBS

) where

import qualified Data.ByteString            as S
import           Data.Int
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Enc
import           Data.Text.Read             (decimal)
import           Network.HTTP.Kinder.Common

-- Class
-- ----------------------------------------------------------------------------

-- | Determines a 'Text' serialization of a type with URI segment
-- representation.
class URIEncode a where
  uriEncode :: a -> Text

-- | Parses a 'Text' serialization of a for some type from a representation
-- as a URI segment.
class URIDecode a where
  uriDecode :: Text -> Either String a

-- | Since 'URIDecode' assumes a 'Text'-like representation but we may read
-- a 'S.ByteString' off the wire, this helper function first tries to
-- decode the 'S.ByteString' as UTF-8 'Text' then decodes according to
-- 'URIDecode'.
uriDecodeBS :: URIDecode a => S.ByteString -> Either String a
uriDecodeBS s = case Enc.decodeUtf8' s of
  Left _err -> Left "could not parse UTF8 string"
  Right a -> uriDecode a

-- Instances
-- ----------------------------------------------------------------------------

instance URIDecode (Raw Text) where
  uriDecode text = Right (Raw text)

-- | For URI segments this instance is the same as the one for @'Raw'
-- 'Text'@ since URI segments don't contain quotations.
instance URIDecode Text where
  uriDecode text = Right text

-- | Decoder for any type with a decimal representation. Requires a 'Show'
-- instance for the error message.
uriDecodeDecimal :: (Show a, Integral a) => Text -> Either String a
uriDecodeDecimal txt =
  case decimal txt of
    Left err -> Left err
    Right (a, t)
      | Text.null t -> Right a
      | otherwise -> Left ("incomplete parse: " ++ show (a, t))

instance URIDecode Int where uriDecode = uriDecodeDecimal
instance URIDecode Int8 where uriDecode = uriDecodeDecimal
instance URIDecode Int16 where uriDecode = uriDecodeDecimal
instance URIDecode Int32 where uriDecode = uriDecodeDecimal
instance URIDecode Int64 where uriDecode = uriDecodeDecimal
instance URIDecode Integer where uriDecode = uriDecodeDecimal
