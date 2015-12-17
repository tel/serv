{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Internal.Header.Serialization where

import qualified Data.ByteString           as S
import qualified Data.CaseInsensitive      as CI
import           Data.Proxy
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Time
import           Network.HTTP.Media        (MediaType, Quality, parseQuality)
import qualified Network.HTTP.Types        as HTTP
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

-- | Handles encoding a header all the way to /raw/ bytes.
headerEncodeRaw :: HeaderEncode n a => Proxy n -> a -> S.ByteString
headerEncodeRaw proxy = Text.encodeUtf8 . headerEncode proxy

instance ReflectName n => HeaderEncode n RawText where
  headerEncode _ (RawText text) = text

instance HeaderEncode 'Allow (Set Verb) where
  headerEncode _ verbs =
    Text.intercalate "," (map (Text.decodeUtf8 . standardName) (Set.toList verbs))

instance HeaderEncode 'Allow [Verb] where
  headerEncode prx verbs = headerEncode prx (Set.fromList verbs)

instance HeaderEncode 'AccessControlExposeHeaders (Set HTTP.HeaderName) where
  headerEncode _ headers =
    Text.intercalate "," (map (Text.decodeUtf8 . CI.original) (Set.toList headers))

instance HeaderEncode 'AccessControlAllowHeaders (Set HTTP.HeaderName) where
  headerEncode _ headers =
    Text.intercalate "," (map (Text.decodeUtf8 . CI.original) (Set.toList headers))

instance HeaderEncode 'AccessControlMaxAge NominalDiffTime where
  headerEncode _ ndt = Text.pack (show (round ndt :: Int))

instance HeaderEncode 'AccessControlAllowOrigin Text where
  headerEncode _ org = org

instance HeaderEncode 'AccessControlAllowMethods (Set Verb) where
  headerEncode _ verbs =
    Text.intercalate "," (map (Text.decodeUtf8 . standardName) (Set.toList verbs))

instance HeaderEncode 'AccessControlAllowCredentials Bool where
  headerEncode _ ok
    | ok = "true"
    | otherwise = "false"

instance {-# OVERLAPPABLE #-} ReflectName n => HeaderEncode n Bool where
  headerEncode _ ok
    | ok = "true"
    | otherwise = "false"

instance {-# OVERLAPPABLE #-} ReflectName n => HeaderEncode n Int where
  headerEncode _ i = Text.pack (show i)

instance {-# OVERLAPPABLE #-} ReflectName n => HeaderEncode n Text where
  headerEncode _ t = t

-- | Represents mechanisms to interpret data types as header-compatible values.
--
-- An instance of @Decode n t@ captures a mechanism for reading values of type
-- @t@ from header data stored at header @n@.
class ReflectName n => HeaderDecode (n :: HeaderName) a where
  headerDecode :: Proxy n -> Text -> Either String a

-- | Handles decoding a header all the way from /raw/ bytes.
headerDecodeRaw :: HeaderDecode n a => Proxy n -> S.ByteString -> Either String a
headerDecodeRaw proxy s = case Text.decodeUtf8' s of
  Left err -> Left (show err)
  Right t -> headerDecode proxy t

-- | 'RawText' enables capturing the data untouched from the header
instance ReflectName n => HeaderDecode n RawText where
  headerDecode _ text = Right (RawText text)

instance HeaderDecode 'Accept [Quality MediaType] where
  headerDecode _ text = case parseQuality (Text.encodeUtf8 text) of
    Nothing -> Left "could not parse media type specification"
    Just qs -> Right qs

headerPair :: HeaderEncode h v => Proxy h -> v -> HTTP.Header
headerPair proxy v = (reflectName proxy, headerEncodeRaw proxy v)
