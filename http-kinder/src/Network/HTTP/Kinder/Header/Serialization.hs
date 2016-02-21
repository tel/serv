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
module Network.HTTP.Kinder.Header.Serialization where

import qualified Data.ByteString                        as S
import           Data.CaseInsensitive                   (CI)
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Singletons
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import qualified Data.Text.Encoding                     as Text
import           Network.HTTP.Kinder.Header.Definitions
import           Network.HTTP.Kinder.Types

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
headerEncodePair sing a = do
  txt <- headerEncode sing a
  return (headerName sing, Text.encodeUtf8 txt)

-- | Interprets a (possibly missing) 'Text' representation for some value
-- taking semantics at a given 'HeaderName'. Any proxy can be passed as the
-- first argument, although 'Sing' is a nice one to choose. If a value is
-- expected and no representation is provided then 'Nothing' can be passed
-- seeking a default value (should one exist).
class HeaderDecode (n :: HeaderName) a where
  headerDecode :: sing n -> Maybe Text -> Either String a

-- Instances/Encoding
-- ----------------------------------------------------------------------------

-- | Output a set of text items as a comma-delimited list OR return nothing
-- if the set is empty
displaySetOpt :: Set Text -> Maybe Text
displaySetOpt s
  | Set.null s = Nothing
  | otherwise = Just (Text.intercalate "," (Set.toList s))

-- | Extend an instance on @'Set' v@ to @[v]@.
uniqueSet :: (Ord v, HeaderEncode n (Set v)) => Sing n -> [v] -> Maybe Text
uniqueSet s = headerEncode s . Set.fromList

-- | Reports a "raw" value without interpretation
instance HeaderEncode n (Raw Text) where
  headerEncode _ (Raw t) = Just t

instance HeaderEncode 'AccessControlAllowCredentials Bool where
  headerEncode _ ok = Just (if ok then "true" else "false")

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
withDefault :: a -> (Text -> Either String a) -> Maybe Text -> Either String a
withDefault def _ Nothing = Right def
withDefault _ f (Just a) = f a

-- | Returns the raw header value
instance HeaderDecode n (Raw Text) where
  headerDecode _ = required $ \text -> Right (Raw text)

-- | Any value may be only optionally captured as desired
instance HeaderDecode h t => HeaderDecode h (Maybe t) where
  headerDecode _ Nothing = Right Nothing
  headerDecode p (Just t) = fmap Just (headerDecode p (Just t))
