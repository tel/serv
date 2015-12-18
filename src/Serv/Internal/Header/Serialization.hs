{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

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
  headerEncode :: Proxy n -> a -> Maybe Text

-- | Handles encoding a header all the way to /raw/ bytes.
headerEncodeRaw :: HeaderEncode n a => Proxy n -> a -> Maybe S.ByteString
headerEncodeRaw proxy = fmap Text.encodeUtf8 . headerEncode proxy

instance ReflectName n => HeaderEncode n RawText where
  headerEncode _ (RawText text) = Just text

instance HeaderEncode 'Allow (Set Verb) where
  headerEncode _ verbs =
    Just $ Text.intercalate "," (map (Text.decodeUtf8 . standardName) (Set.toList verbs))

instance HeaderEncode 'Allow [Verb] where
  headerEncode prx verbs = headerEncode prx (Set.fromList verbs)

instance HeaderEncode 'AccessControlExposeHeaders (Set HTTP.HeaderName) where
  headerEncode _ headers
    | Set.null headers = Nothing
    | otherwise =
        Just $ Text.intercalate ","
               (map (Text.decodeUtf8 . CI.original) (Set.toList headers))

instance HeaderEncode 'AccessControlAllowHeaders (Set HTTP.HeaderName) where
  headerEncode _ headers
    | Set.null headers = Nothing
    | otherwise =
        Just $ Text.intercalate ","
               (map (Text.decodeUtf8 . CI.original) (Set.toList headers))

instance HeaderEncode 'AccessControlMaxAge NominalDiffTime where
  headerEncode _ ndt = Just $ Text.pack (show (round ndt :: Int))

instance HeaderEncode 'AccessControlAllowOrigin Text where
  headerEncode _ org = Just org

instance HeaderEncode 'AccessControlAllowMethods (Set Verb) where
  headerEncode _ verbs
    | Set.null verbs = Nothing
    | otherwise =
        Just $ Text.intercalate ","
               (map (Text.decodeUtf8 . standardName) (Set.toList verbs))

instance HeaderEncode 'AccessControlAllowMethods [Verb] where
  headerEncode prx verbs = headerEncode prx (Set.fromList verbs)

instance HeaderEncode 'AccessControlAllowCredentials Bool where
  headerEncode _ ok
    | ok = Just "true"
    | otherwise = Just "false"

instance {-# OVERLAPPABLE #-} ReflectName n => HeaderEncode n Bool where
  headerEncode _ ok
    | ok = Just "true"
    | otherwise = Just "false"

instance {-# OVERLAPPABLE #-} ReflectName n => HeaderEncode n Int where
  headerEncode _ i = Just $ Text.pack (show i)

instance {-# OVERLAPPABLE #-} ReflectName n => HeaderEncode n Text where
  headerEncode _ t = Just t

instance {-# OVERLAPPABLE #-} HeaderEncode h t => HeaderEncode h (Maybe t) where
  headerEncode p v = v >>= headerEncode p

-- | Represents mechanisms to interpret data types as header-compatible values.
--
-- An instance of @Decode n t@ captures a mechanism for reading values of type
-- @t@ from header data stored at header @n@.
class ReflectName n => HeaderDecode (n :: HeaderName) a where
  headerDecode :: Proxy n -> Maybe Text -> Either String a

headerDecode' :: HeaderDecode n a => Proxy n -> Text -> Either String a
headerDecode' p = headerDecode p . Just

required :: (Text -> Either String a) -> Maybe Text -> Either String a
required _ Nothing = Left "missing header value"
required f (Just t) = f t

-- | Handles decoding a header all the way from /raw/ bytes.
headerDecodeRaw :: HeaderDecode n a => Proxy n -> Maybe S.ByteString -> Either String a
headerDecodeRaw proxy mays =
  case mays of
    Nothing -> headerDecode proxy Nothing
    Just s ->
      case Text.decodeUtf8' s of
        Left err -> Left (show err)
        Right t -> headerDecode' proxy t

-- | 'RawText' enables capturing the data untouched from the header
instance ReflectName n => HeaderDecode n RawText where
  headerDecode _ = required $ \text -> Right (RawText text)

instance HeaderDecode 'Accept [Quality MediaType] where
  headerDecode _ Nothing = Right []
  headerDecode _ (Just text) =
    case parseQuality (Text.encodeUtf8 text) of
      Nothing -> Left "could not parse media type specification"
      Just qs -> Right qs

instance {-# OVERLAPPABLE #-} HeaderDecode h t => HeaderDecode h (Maybe t) where
  headerDecode _ Nothing = Right Nothing
  headerDecode p (Just t) = fmap Just (headerDecode' p t)

headerPair :: HeaderEncode h v => Proxy h -> v -> Maybe HTTP.Header
headerPair proxy v = fmap (reflectName proxy,) (headerEncodeRaw proxy v)
