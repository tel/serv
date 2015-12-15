{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Serv.Internal.Verb where

import           Data.Proxy
import qualified Network.HTTP.Types as HTTP

-- TRACE is intentionally omitted because (a) it's very low value and (b)
-- it opens a potential security hole via Cross-Site-Tracing. Instead of
-- even evaluating that risk we'll just disallow it.

data Verb
  = GET
  | HEAD
  | POST
  | PUT
  | PATCH
  | DELETE
  | OPTIONS
    deriving ( Eq, Ord, Show, Read )

standardName :: Verb -> HTTP.Method
standardName v = case v of
  GET -> HTTP.methodGet
  HEAD -> HTTP.methodHead
  POST -> HTTP.methodPost
  PUT -> HTTP.methodPut
  PATCH -> HTTP.methodPatch
  DELETE -> HTTP.methodDelete
  OPTIONS -> HTTP.methodOptions

class ReflectVerb (v :: Verb) where
  reflectVerb :: Proxy v -> Verb

instance ReflectVerb 'GET where reflectVerb Proxy = GET
instance ReflectVerb 'HEAD where reflectVerb Proxy = HEAD
instance ReflectVerb 'POST where reflectVerb Proxy = POST
instance ReflectVerb 'PUT where reflectVerb Proxy = PUT
instance ReflectVerb 'PATCH where reflectVerb Proxy = PATCH
instance ReflectVerb 'DELETE where reflectVerb Proxy = DELETE
instance ReflectVerb 'OPTIONS where reflectVerb Proxy = OPTIONS
