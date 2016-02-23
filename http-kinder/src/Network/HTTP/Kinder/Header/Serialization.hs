{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | 'HeaderName's define semantics for 'Text' values seen in HTTP headers
-- over the wire. This module provides classes to map both to and from
-- these reprsentations.
module Network.HTTP.Kinder.Header.Serialization (


  -- * Classes for encoding and decoding

    HeaderEncode (..)
  , HeaderDecode (..)

  -- * Extra serialization utilities

  , headerEncodePair
  , headerEncodeBS
  , headerDecodeBS

  -- * Utilities for writing serialization instances

  , displaySetOpt
  , uniqueSet
  , required
  , withDefault

) where

import qualified Data.ByteString                        as S
import           Data.CaseInsensitive                   (CI)
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Singletons
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import qualified Data.Text.Encoding                     as Text
import           Data.Time
import           Network.HTTP.Kinder.Common
import           Network.HTTP.Kinder.Header.Definitions
import           Network.HTTP.Kinder.Verb
import           Network.HTTP.Media                     (MediaType, Quality)
import qualified Network.HTTP.Media                     as Media

-- | Determines a 'Text' representation for some value to be encoded as
-- a value of a given 'HeaderName'. Any proxy can be passed as the first
-- argument, although 'Sing' is a nice one to choose. Encodings may choose
-- to not be represented on the wire at all as desired by returning
-- 'Nothing'. This implies default behavior.
class HeaderEncode (n :: HeaderName) a where
  headerEncode :: sing n -> a -> Maybe Text

-- | Encode a 'HeaderName' singleton and a 'HeaderEncode'-represented value
-- as a pair of name and representation, ready to be sent over the wire.
headerEncodePair
  :: forall a (n :: HeaderName)
  . HeaderEncode n a => Sing n -> a -> Maybe (CI S.ByteString, S.ByteString)
headerEncodePair s a = do
  bs <- headerEncodeBS s a
  return (headerName s, bs)

-- | While the semantics of HTTP headers are built off of 'Text'-like
-- values, usually we require a 'S.ByteString' for emission. This helper
-- function converts a header value directly to a 'S.ByteString'.
headerEncodeBS :: HeaderEncode n a => sing n -> a -> Maybe S.ByteString
headerEncodeBS s = fmap Text.encodeUtf8 . headerEncode s

-- | Interprets a (possibly missing) 'Text' representation for some value
-- taking semantics at a given 'HeaderName'. Any proxy can be passed as the
-- first argument, although 'Sing' is a nice one to choose. If a value is
-- expected and no representation is provided then 'Nothing' can be passed
-- seeking a default value (should one exist).
class HeaderDecode (n :: HeaderName) a where
  headerDecode :: sing n -> Maybe Text -> Either String a

-- | While HTTP header semantics are built off of 'Text'-like values, we
-- usually read a raw 'S.ByteString' from the wire. This helper function
-- combines a 'HeaderDecode' with a UTF-8 decode so as to attempt to
-- deserialize header values directly from a 'S.ByteString'.
headerDecodeBS :: HeaderDecode n a => sing n -> Maybe S.ByteString -> Either String a
headerDecodeBS proxy mays =
  case mays of
    Nothing -> headerDecode proxy Nothing
    Just s ->
      case Text.decodeUtf8' s of
        Left err -> Left (show err)
        Right t -> headerDecode proxy (Just t)

-- Instances/Encoding
-- ----------------------------------------------------------------------------

-- | Output a set of text items as a comma-delimited list OR return nothing
-- if the set is empty
displaySetOpt :: Set Text -> Maybe Text
displaySetOpt s
  | Set.null s = Nothing
  | otherwise = Just (Text.intercalate "," (Set.toList s))

-- | Extend a 'HeaderEncode' instance on @'Set' v@ to @[v]@.
uniqueSet :: (Ord v, HeaderEncode n (Set v)) => sing n -> [v] -> Maybe Text
uniqueSet s = headerEncode s . Set.fromList

-- | Reports a "raw" value without interpretation
instance HeaderEncode n (Raw Text) where
  headerEncode _ (Raw t) = Just t

instance HeaderEncode 'Allow (Set Verb) where
  headerEncode _ = displaySetOpt . Set.map verbName

instance HeaderEncode 'Allow [Verb] where
  headerEncode = uniqueSet

instance HeaderEncode 'AccessControlExposeHeaders (Set SomeHeaderName) where
  headerEncode _ = displaySetOpt . Set.map headerName' where
    headerName' (SomeHeaderName h) = headerName h

instance HeaderEncode 'AccessControlExposeHeaders [SomeHeaderName] where
  headerEncode = uniqueSet

instance HeaderEncode 'AccessControlAllowHeaders (Set SomeHeaderName) where
  headerEncode _ = displaySetOpt . Set.map headerName' where
    headerName' (SomeHeaderName h) = headerName h

instance HeaderEncode 'AccessControlAllowHeaders [SomeHeaderName] where
  headerEncode = uniqueSet

instance HeaderEncode 'AccessControlMaxAge NominalDiffTime where
  headerEncode _ ndt = Just $ Text.pack (show (round ndt :: Int))

instance HeaderEncode 'AccessControlAllowOrigin Text where
  headerEncode _ org = Just org

instance HeaderEncode 'AccessControlAllowMethods (Set Verb) where
  headerEncode _ = displaySetOpt . Set.map verbName

instance HeaderEncode 'AccessControlAllowMethods [Verb] where
  headerEncode = uniqueSet

instance HeaderEncode 'AccessControlAllowCredentials Bool where
  headerEncode _ ok = Just (if ok then "true" else "false")

instance HeaderEncode 'ContentType MediaType where
  headerEncode _ mt =
    case Text.decodeUtf8' (Media.renderHeader mt) of
      Left _err -> Nothing
      Right txt -> Just txt

-- | Any value can be forced as optional if desired
instance HeaderEncode h t => HeaderEncode h (Maybe t) where
  headerEncode p v = v >>= headerEncode p

-- Instances/Decoding
-- ----------------------------------------------------------------------------

-- | Fail to decode if there is no header. For headers which lack default
-- values. If a header lacks a natural default then avoiding failure should
-- be /explicitly/ requested in the types by wrapping it with a 'Maybe'.
required :: (Text -> Either String a) -> Maybe Text -> Either String a
required _ Nothing = Left "missing header value"
required f (Just t) = f t

-- | For headers with natural notions of default values.
withDefault :: a -> (Text -> Either String a) -> (Maybe Text -> Either String a)
withDefault def _ Nothing = Right def
withDefault _ f (Just a) = f a

instance HeaderDecode Accept [Quality MediaType] where
  headerDecode _ = withDefault [] parser where
    parser txt =
      case Media.parseQuality (Text.encodeUtf8 txt) of
        Nothing -> Left "malformed accept header"
        Just mts -> Right mts

instance HeaderDecode ContentType MediaType where
  headerDecode _ = required $ \txt ->
    case Media.parseAccept (Text.encodeUtf8 txt) of
      Nothing -> Left "malformed content type"
      Just ct -> Right ct

-- | Returns the raw header value
instance HeaderDecode n (Raw Text) where
  headerDecode _ = required $ \text -> Right (Raw text)

-- | Any value may be only optionally captured as desired
instance HeaderDecode h t => HeaderDecode h (Maybe t) where
  headerDecode _ Nothing = Right Nothing
  headerDecode p (Just t) = fmap Just (headerDecode p (Just t))
