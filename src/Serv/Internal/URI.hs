
module Serv.Internal.URI where

import           Control.Monad (liftM)
import qualified Data.ByteString.Char8 as S8
import           Data.Text             (Text)
import           Data.Text.Read (decimal)
import qualified Data.Text.Encoding    as Enc
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

instance URIDecode Int where
  uriDecode = liftM fst . decimal
