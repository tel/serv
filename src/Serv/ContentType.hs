{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.ContentType (

    HasMediaType (..)
  , MimeEncode (..)
  , MimeDecode (..)
  , (//)
  , (/:)

    -- Cheating data types
  , RawText (..)

    -- Standard Content Types
  , TextPlain

  ) where

import qualified Data.Text.Encoding      as Text
import           Network.HTTP.Media
import           Serv.Internal.MediaType
import           Serv.Internal.RawText

data TextPlain

instance HasMediaType TextPlain where
  mediaType _ = "text" // "plain"

instance MimeEncode TextPlain RawText where
  mimeEncode _ (RawText t) = Text.encodeUtf8 t
