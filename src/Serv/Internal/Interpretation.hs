{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Serv.Internal.Interpretation where

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8
import           Data.Proxy
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as Text
import           Network.HTTP.Media    (MediaType)
import           Serv.Internal.Header


-- Raw Text
-- ----------------------------------------------------------------------------

-- | RawText extracts as, like the name suggests,
-- raw text from URI segments and header values.
--
-- It exists as a default value for extensibility of typeclasses like
-- HeaderDecode and HeaderEncode

newtype RawText =
  RawText { getRawText :: Text }
  deriving (Eq, Ord, Read, Show, Monoid)





-- URIs
-- ----------------------------------------------------------------------------

class URIEncode a where
  uriEncode :: a -> Text

class URIDecode a where
  uriDecode :: Text -> Either String a

instance URIDecode RawText where
  uriDecode text = Right (RawText text)

fromByteString :: URIDecode a => S8.ByteString -> Either String a
fromByteString s = case Text.decodeUtf8' s of
  Left _err -> Left "could not parse UTF8 string"
  Right a -> uriDecode a






-- MediaTypes
-- ----------------------------------------------------------------------------
class HasMediaType ty where
  mediaType :: Proxy ty -> MediaType

class HasMediaType ty => MimeEncode ty val where
  mimeEncode :: Proxy ty -> val -> S.ByteString

class HasMediaType ty => MimeDecode ty val where
  mimeDecode :: Proxy ty -> S.ByteString -> Either String val






-- Headers
-- ----------------------------------------------------------------------------

-- | Represents mechanisms to interpret data types as header-compatible values.
--
-- An instance of @Encode n t@ captures a mechanism for treating values of type
-- @t@ as valid data to substantiate the header @n@.
class HeaderEncode (n :: Name) a where
  headerEncode :: Proxy n -> a -> Text

instance HeaderEncode (n :: Name) RawText where
  headerEncode _ (RawText text) = text

-- | Represents mechanisms to interpret data types as header-compatible values.
--
-- An instance of @Decode n t@ captures a mechanism for reading values of type
-- @t@ from header data stored at header @n@.
class HeaderDecode (n :: Name) a where
  headerDecode :: Proxy n -> Text -> Either String a

-- | 'RawText' enables capturing the data untouched from the header
instance HeaderDecode n RawText where
  headerDecode _ text = Right (RawText text)
