
module Serv.Internal.URI where

import qualified Data.ByteString.Char8 as S8
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as Enc
import           Serv.Internal.RawText

class URIEncode a where
  uriEncode :: a -> Text

class URIDecode a where
  uriDecode :: Text -> Either String a

instance URIDecode RawText where
  uriDecode text = Right (RawText text)

fromByteString :: URIDecode a => S8.ByteString -> Either String a
fromByteString s = case Enc.decodeUtf8' s of
  Left _err -> Left "could not parse UTF8 string"
  Right a -> uriDecode a
