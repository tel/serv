
module Serv.ContentType (

    HasMediaType (..)
  , MimeEncode (..)
  , MimeDecode (..)
  , (//)
  , (/:)

  ) where

import Serv.Internal.MediaType
import Network.HTTP.Media
