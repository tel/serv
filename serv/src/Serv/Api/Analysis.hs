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
module Serv.Api.Analysis where

import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Tuple
import           Data.Text                    (Text)
import           GHC.TypeLits
import           Serv.Api
import           Network.HTTP.Kinder.Header         (SomeHeaderName (..), HeaderName)
import           Network.HTTP.Kinder.Verb (Verb)

data EndpointAnalysis
  = EndpointAnalysis
    { verbsHandled :: Set Verb
    , headersExpected :: Set SomeHeaderName
    , headersEmitted :: Set SomeHeaderName
    }

instance Monoid EndpointAnalysis where
  mempty = EndpointAnalysis mempty mempty mempty
  mappend ea eb =
    EndpointAnalysis
    { verbsHandled = verbsHandled ea <> verbsHandled eb
    , headersExpected = headersExpected ea <> headersExpected eb
    , headersEmitted = headersEmitted ea <> headersEmitted eb
    }

inspectEndpoint :: forall (hs :: [Handler *]) . Sing hs -> EndpointAnalysis
inspectEndpoint s =
  case s of
    SNil -> mempty
    SCons sHandler sRest -> inspectHandler sHandler <> inspectEndpoint sRest

inspectHandler :: forall (h :: Handler *) . Sing h -> EndpointAnalysis
inspectHandler s =
  case s of
    SCaptureQuery _ sNext -> inspectHandler sNext
    SCaptureBody _ _ sNext -> inspectHandler sNext
    SMethod sVerb sResponses ->
      case sResponses of
        SNil -> mempty
        SCons (STuple2 _sCode (SRespond sHdrs _sBody)) sRest ->
          EndpointAnalysis
          { verbsHandled = Set.singleton (fromSing sVerb)
          , headersEmitted = headerNames sHdrs
          , headersExpected = Set.empty
          } <> inspectHandler (SMethod sVerb sRest)
    SCaptureHeaders sHdrs sNext ->
      EndpointAnalysis
      { verbsHandled = Set.empty
      , headersEmitted = Set.empty
      , headersExpected = headerNames sHdrs
      }
      <> inspectHandler sNext

headerNames :: forall (hts :: [(HeaderName, k)]) . Sing hts -> Set SomeHeaderName
headerNames s =
  case s of
    SNil -> Set.empty
    SCons (STuple2 sHt _sTy) sRest ->
      Set.insert (SomeHeaderName sHt) (headerNames sRest)

inspectVerbs :: forall (hs :: [Handler *]) . Sing hs -> Set Verb
inspectVerbs = verbsHandled . inspectEndpoint

headersExpectedOf :: forall (hs :: [Handler *]) . Sing hs -> Set SomeHeaderName
headersExpectedOf = headersExpected . inspectEndpoint
