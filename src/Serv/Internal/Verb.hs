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

standardName :: IsString t => Verb -> t
standardName v =
  case v of
    GET -> "GET"
    HEAD -> "HEAD"
    POST -> "POST"
    PUT -> "PUT"
    PATCH -> "PATCH"
    DELETE -> "DELETE"
    OPTIONS -> "OPTIONS"
