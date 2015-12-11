{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Serv.Internal.Verb where

import           Data.Proxy
import qualified Network.HTTP.Types as HTTP

data Verb
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
    deriving ( Eq, Ord, Show, Read )

standardName :: Verb -> HTTP.Method
standardName v = case v of
  GET -> HTTP.methodGet
  POST -> HTTP.methodPost
  PUT -> HTTP.methodPut
  PATCH -> HTTP.methodPatch
  DELETE -> HTTP.methodDelete

class ReflectVerb (v :: Verb) where
  reflectVerb :: Proxy v -> Verb

instance ReflectVerb 'GET where reflectVerb Proxy = GET
instance ReflectVerb 'POST where reflectVerb Proxy = POST
instance ReflectVerb 'PUT where reflectVerb Proxy = PUT
instance ReflectVerb 'PATCH where reflectVerb Proxy = PATCH
instance ReflectVerb 'DELETE where reflectVerb Proxy = DELETE
