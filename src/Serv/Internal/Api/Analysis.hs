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
module Serv.Internal.Api.Analysis where

import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Tuple
import           Data.Text                    (Text)
import           GHC.TypeLits
import           Serv.Internal.Api
import           Serv.Internal.Header         (HeaderType)
import qualified Serv.Internal.Header         as Header
import           Serv.Internal.Verb

data EndpointAnalysis
  = EndpointAnalysis
    { verbsHandled :: Set Verb
    , headersExpected :: Set (HeaderType Text)
    , headersEmitted :: Set (HeaderType Text)
    }

instance Monoid EndpointAnalysis where
  mempty = EndpointAnalysis mempty mempty mempty
  mappend ea eb =
    EndpointAnalysis
    { verbsHandled = verbsHandled ea <> verbsHandled eb
    , headersExpected = headersExpected ea <> headersExpected eb
    , headersEmitted = headersEmitted ea <> headersEmitted eb
    }

inspectEndpoint :: forall (hs :: [Handler Symbol *]) . Sing hs -> EndpointAnalysis
inspectEndpoint s =
  case s of
    SNil -> mempty
    SCons sHandler sRest -> inspectHandler sHandler <> inspectEndpoint sRest

inspectHandler :: forall (h :: Handler Symbol *) . Sing h -> EndpointAnalysis
inspectHandler s =
  case s of
    SCaptureQuery _ sNext -> inspectHandler sNext
    SCaptureBody _ _ sNext -> inspectHandler sNext
    SMethod sVerb sHdrs _sBody ->
      EndpointAnalysis
      { verbsHandled = Set.singleton (fromSing sVerb)
      , headersEmitted = headerNames sHdrs
      , headersExpected = Set.empty
      }
    SCaptureHeaders sHdrs sNext ->
      EndpointAnalysis
      { verbsHandled = Set.empty
      , headersEmitted = Set.empty
      , headersExpected = headerNames sHdrs
      }
      <> inspectHandler sNext

headerNames :: forall (hts :: [ (HeaderType Symbol, k) ]) . Sing hts -> Set (HeaderType Text)
headerNames s =
  case s of
    SNil -> Set.empty
    SCons (STuple2 sHt _sTy) sRest ->
      Set.insert (Header.headerType sHt) (headerNames sRest)

inspectVerbs :: forall (hs :: [Handler Symbol *]) . Sing hs -> Set Verb
inspectVerbs = verbsHandled . inspectEndpoint

headersExpectedOf :: forall (hs :: [Handler Symbol *]) . Sing hs -> Set (HeaderType Text)
headersExpectedOf = headersExpected . inspectEndpoint
