{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Typeclasses constructing functions which reflect and analyze API
-- types.
module Serv.Internal.Api.Analysis where

import           Data.Function        ((&))
import           Data.Monoid
import           Data.Proxy
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Network.HTTP.Types   as HTTP
import           Serv.Internal.Api
import qualified Serv.Internal.Header as Header
import qualified Serv.Internal.Verb   as Verb

class VerbsOf methods where
  verbsOf :: Proxy methods -> Set Verb.Verb

instance VerbsOf '[] where
  verbsOf Proxy = Set.singleton Verb.OPTIONS

instance
  (Verb.ReflectVerb verb, VerbsOf methods) =>
    VerbsOf ('Method verb headers body ': methods)
  where
    verbsOf Proxy =
      case Verb.reflectVerb (Proxy :: Proxy verb) of
        Verb.GET ->
          verbsOf (Proxy :: Proxy methods)
          & Set.insert Verb.GET
          & Set.insert Verb.HEAD
        verb -> Set.insert verb (verbsOf (Proxy :: Proxy methods))

class HeadersExpectedOf (methods :: [Method *]) where
  headersExpectedOf :: Proxy methods -> Set HTTP.HeaderName

instance HeadersExpectedOf '[] where
  headersExpectedOf _ = Set.empty

instance
  HeadersExpectedOf rs => HeadersExpectedOf ('Method verb headers body ': rs)
  where
    -- No headers are expected from a bottom method
    headersExpectedOf _ = headersExpectedOf (Proxy :: Proxy rs)

instance
  (HeadersExpectedOf (method ': rs), Header.ReflectHeaderNames hdrs) =>
  HeadersExpectedOf ('CaptureHeaders hdrs method ': rs)
  where
    headersExpectedOf _ =
      Set.fromList (Header.reflectHeaderNames (Proxy :: Proxy hdrs))
      <>
      headersExpectedOf (Proxy :: Proxy (method ': rs))

instance
  HeadersExpectedOf (method ': rs) =>
  HeadersExpectedOf ('CaptureBody ctypes ty method ': rs)
  where
    headersExpectedOf _ =
      headersExpectedOf (Proxy :: Proxy (method ': rs))

instance
  HeadersExpectedOf (method ': rs) =>
  HeadersExpectedOf ('CaptureQuery names method ': rs)
  where
    headersExpectedOf _ =
      headersExpectedOf (Proxy :: Proxy (method ': rs))

class HeadersReturnedBy (methods :: [Method *]) where
  headersReturnedBy :: Proxy methods -> Set HTTP.HeaderName

instance HeadersReturnedBy '[] where
  headersReturnedBy _ = Set.empty

instance
  (Header.ReflectHeaderNames headers, HeadersReturnedBy rs) =>
  HeadersReturnedBy ('Method verb headers body ': rs)
  where
    headersReturnedBy _ =
      Set.fromList (Header.reflectHeaderNames (Proxy :: Proxy headers))
      <>
      headersReturnedBy (Proxy :: Proxy rs)

instance
  HeadersReturnedBy (method ': rs) =>
  HeadersReturnedBy ('CaptureHeaders hdrs method ': rs)
  where
    headersReturnedBy _ =
      headersReturnedBy (Proxy :: Proxy (method ': rs))

instance
  HeadersReturnedBy (method ': rs) =>
  HeadersReturnedBy ('CaptureBody ctypes ty method ': rs)
  where
    headersReturnedBy _ =
      headersReturnedBy (Proxy :: Proxy (method ': rs))

instance
  HeadersReturnedBy (method ': rs) =>
  HeadersReturnedBy ('CaptureQuery names method ': rs)
  where
    headersReturnedBy _ =
      headersReturnedBy (Proxy :: Proxy (method ': rs))
