{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Serv.Internal.Verb where

import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.TH
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

-- | Convert a 'Verb' to its http-types form
standardName :: Verb -> HTTP.Method
standardName v = case v of
  GET -> HTTP.methodGet
  HEAD -> HTTP.methodHead
  POST -> HTTP.methodPost
  PUT -> HTTP.methodPut
  PATCH -> HTTP.methodPatch
  DELETE -> HTTP.methodDelete
  OPTIONS -> HTTP.methodOptions
