{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.ContentType (

    HasMediaType (..)
  , MimeEncode (..)
  , MimeDecode (..)
  , (//)
  , (/:)

    -- Standard Content Types
  , TextPlain

  ) where

import           Data.Text               (Text)
import qualified Data.Text.Encoding      as Text
import           Network.HTTP.Media
import           Serv.Internal.MediaType

data TextPlain

instance HasMediaType TextPlain where
  mediaType _ = "text" // "plain"

instance MimeEncode TextPlain Text where
  mimeEncode _ = Text.encodeUtf8
