{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Serv.Internal.Verb where

import           Data.Singletons
import           Data.Singletons.TH
import           Data.String
import           Data.Text
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as S
import qualified Data.CaseInsensitive as CI

-- TRACE is intentionally omitted because (a) it's very low value and (b)
-- it opens a potential security hole via Cross-Site-Tracing. Instead of
-- even evaluating that risk we'll just disallow it.

singletons
  [d|
    data Verb
      = DELETE
      | GET
      | HEAD
      | OPTIONS
      | PATCH
      | POST
      | PUT
        deriving ( Eq, Ord, Show, Read )
  |]

verbName :: IsString t => Verb -> t
verbName v =
  case v of
    GET -> "GET"
    HEAD -> "HEAD"
    POST -> "POST"
    PUT -> "PUT"
    PATCH -> "PATCH"
    DELETE -> "DELETE"
    OPTIONS -> "OPTIONS"

parseVerb :: S.ByteString -> Maybe Verb
parseVerb s =
  case CI.mk s of
    "GET" -> Just GET
    "HEAD" -> Just HEAD
    "POST" -> Just POST
    "PUT" -> Just PUT
    "PATCH" -> Just PATCH
    "DELETE" -> Just DELETE
    "OPTIONS" -> Just OPTIONS
    _ -> Nothing
