{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Serv.Internal.Interpretation where

import           Data.Proxy
import           Data.Text (Text)
import           Network.HTTP.Media (MediaType)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text.Encoding as Text


newtype RawText = RawText { getRawText :: Text }

class URIEncode a where
  uriEncode :: a -> Text

class URIDecode a where
  uriDecode :: Text -> Either String a

instance URIDecode RawText where
  uriDecode text = Right (RawText text)

class HeaderEncode a where
  headerEncode :: a -> Text

class HeaderDecode a where
  headerDecode :: Text -> Either String a

fromByteString :: URIDecode a => S8.ByteString -> Either String a
fromByteString s = case Text.decodeUtf8' s of
  Left _err -> Left "could not parse UTF8 string"
  Right a -> uriDecode a

class HasMediaType ty where
  mediaType :: Proxy ty -> MediaType

class HasMediaType ty => MimeEncode ty val where
  mimeEncode :: Proxy ty -> val -> S.ByteString

class HasMediaType ty => MimeDecode ty val where
  mimeDecode :: Proxy ty -> S.ByteString -> Either String val
