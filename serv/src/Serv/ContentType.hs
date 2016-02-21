{-# LANGUAGE FlexibleInstances     #-}
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
  , JSON

  ) where

import qualified Data.Aeson              as A
import qualified Data.ByteString.Lazy    as Sl
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as Text
import           Network.HTTP.Media
import           Serv.Internal.MediaType

data TextPlain

instance HasMediaType TextPlain where
  mediaType _ = "text" // "plain"

instance MimeEncode Text TextPlain where
  mimeEncode _ = Text.encodeUtf8

data JSON

instance HasMediaType JSON where
  mediaType _ = "application" // "json"

instance A.ToJSON a => MimeEncode a JSON where
  mimeEncode _ = Sl.toStrict . A.encode

instance A.FromJSON a => MimeDecode a JSON where
  mimeDecode _ bs = A.eitherDecodeStrict bs
