{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Internal.Header.Serialization where

import qualified Data.ByteString           as S
import           Data.Proxy
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Network.HTTP.Media        (MediaType, Quality, parseQuality)
import           Serv.Internal.Header.Name
import           Serv.Internal.RawText
import           Serv.Internal.Verb

-- | Represents mechanisms to interpret data types as header-compatible values.
--
-- An instance of @Encode n t@ captures a mechanism for treating values of type
-- @t@ as valid data to substantiate the header @n@.
--
-- Note: While this class allows the encoding of any value into a full Unicode
-- Text value, Headers do not generally accept most Unicode code points. Be
-- conservative in implementing this class.
class ReflectName n => HeaderEncode (n :: HeaderName) a where
  headerEncode :: Proxy n -> a -> Text

headerEncodeBS :: HeaderEncode n a => Proxy n -> a -> S.ByteString
headerEncodeBS proxy = Text.encodeUtf8 . headerEncode proxy

instance ReflectName n => HeaderEncode (n :: HeaderName) RawText where
  headerEncode _ (RawText text) = text

instance HeaderEncode 'Allow [Verb] where
  headerEncode _ verbs = Text.intercalate "," (map (Text.pack . show) verbs)


-- | Represents mechanisms to interpret data types as header-compatible values.
--
-- An instance of @Decode n t@ captures a mechanism for reading values of type
-- @t@ from header data stored at header @n@.
class ReflectName n => HeaderDecode (n :: HeaderName) a where
  headerDecode :: Proxy n -> Text -> Either String a

headerDecodeBS :: HeaderDecode n a => Proxy n -> S.ByteString -> Either String a
headerDecodeBS proxy s = case Text.decodeUtf8' s of
  Left err -> Left (show err)
  Right t -> headerDecode proxy t

-- | 'RawText' enables capturing the data untouched from the header
instance ReflectName n => HeaderDecode n RawText where
  headerDecode _ text = Right (RawText text)

instance HeaderDecode 'Accept [Quality MediaType] where
  headerDecode _ text = case parseQuality (Text.encodeUtf8 text) of
    Nothing -> Left "could not parse media type specification"
    Just qs -> Right qs
