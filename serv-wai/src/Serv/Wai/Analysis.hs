{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Typeclasses constructing functions which reflect and analyze API
-- types.
--
-- (/A little rough right now, sorry/)
module Serv.Wai.Analysis where

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Singletons
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Tuple
import Network.HTTP.Kinder.Header (HeaderName, SomeHeaderName (..))
import Network.HTTP.Kinder.Verb (Verb)
import Serv.Api

data EndpointAnalysis
  = EndpointAnalysis
    { verbsHandled    :: Set Verb
    , headersExpected :: Set SomeHeaderName
    , headersEmitted  :: Set SomeHeaderName
    }

instance Semigroup EndpointAnalysis where
  ea <> eb =
    EndpointAnalysis
    { verbsHandled = verbsHandled ea <> verbsHandled eb
    , headersExpected = headersExpected ea <> headersExpected eb
    , headersEmitted = headersEmitted ea <> headersEmitted eb
    }

instance Monoid EndpointAnalysis where
  mempty  = EndpointAnalysis mempty mempty mempty
  mappend x y = x <> y


inspectEndpoint :: forall (hs :: [(Verb, Handler *)]) . Sing hs -> EndpointAnalysis
inspectEndpoint s =
  case s of
    SNil -> mempty
    SCons (STuple2 sVerb sHandler) sRest ->
      inspectHandler sVerb sHandler <> inspectEndpoint sRest

inspectHandler :: forall (v :: Verb) (h :: Handler *) . Sing v -> Sing h -> EndpointAnalysis
inspectHandler sVerb s =
  case s of
    SCaptureQuery _ sNext -> inspectHandler sVerb sNext
    SCaptureBody _ _ sNext -> inspectHandler sVerb sNext
    SOutputs sResponses ->
      case sResponses of
        SNil -> mempty
        SCons (STuple2 _sCode (SRespond sHdrs _sBody)) sRest ->
          EndpointAnalysis
          { verbsHandled = Set.singleton (fromSing sVerb)
          , headersEmitted = headerNames sHdrs
          , headersExpected = Set.empty
          } <> inspectHandler sVerb (SOutputs sRest)
    SCaptureHeaders sHdrs sNext ->
      EndpointAnalysis
      { verbsHandled = Set.empty
      , headersEmitted = Set.empty
      , headersExpected = headerNames sHdrs
      }
      <> inspectHandler sVerb sNext

headerNames :: forall (hts :: [(HeaderName, k)]) . Sing hts -> Set SomeHeaderName
headerNames s =
  case s of
    SNil -> Set.empty
    SCons (STuple2 sHt _sTy) sRest ->
      Set.insert (SomeHeaderName sHt) (headerNames sRest)

inspectVerbs :: forall (hs :: [(Verb, Handler *)]) . Sing hs -> Set Verb
inspectVerbs = verbsHandled . inspectEndpoint

headersExpectedOf :: forall (hs :: [(Verb, Handler *)]) . Sing hs -> Set SomeHeaderName
headersExpectedOf = headersExpected . inspectEndpoint
