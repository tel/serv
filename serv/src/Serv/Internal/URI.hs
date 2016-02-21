
module Serv.Internal.URI where

import qualified Data.ByteString.Char8 as S8
import           Data.Int
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Enc
import           Data.Text.Read        (decimal)
import           Serv.Internal.RawText

-- Class
-- ----------------------------------------------------------------------------

class URIEncode a where
  uriEncode :: a -> Text

class URIDecode a where
  uriDecode :: Text -> Either String a

fromByteString :: URIDecode a => S8.ByteString -> Either String a
fromByteString s = case Enc.decodeUtf8' s of
  Left _err -> Left "could not parse UTF8 string"
  Right a -> uriDecode a

-- Instances
-- ----------------------------------------------------------------------------

instance URIDecode RawText where
  uriDecode text = Right (RawText text)

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
