{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Convert Serv Apis to Swagger definitions
module Serv.Swagger where

import           Control.Lens
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.Swagger
import qualified Network.HTTP.Types.Status    as S
import           Serv.Internal.Api
import           Serv.Internal.StatusCode
import           Serv.Internal.Verb

class HasExample a where
  anExample :: Maybe a

opOfVerb :: forall (v :: Verb) . Sing v -> Lens' PathItem (Maybe Operation)
opOfVerb v =
  case v of
    SGET -> pathItemGet
    SDELETE -> pathItemDelete
    SHEAD -> pathItemHead
    SOPTIONS -> pathItemOptions
    SPATCH -> pathItemPatch
    SPOST -> pathItemPost
    SPUT-> pathItemPut

pathItem :: forall (v :: Verb) . Sing v -> Operation -> PathItem
pathItem v op = mempty & opOfVerb v .~ Just op

swResponses :: forall (rs :: [ (StatusCode Nat, Output Symbol *) ]) . Sing rs -> Responses
swResponses SNil = mempty
swResponses (SCons (STuple2 code out) rest) =
  let cont = swResponses rest
      statusNum = S.statusCode (httpStatus code)
      resp = mempty
  in cont & responsesResponses . ix statusNum .~ Inline resp

swHandler :: forall (h :: Handler Nat Symbol *) . Sing h -> PathItem
swHandler h =
  case h of
    SMethod verb responses ->
      let resps =
            mempty
              & ix 200 .~ Inline mempty
          op =
            mempty
              & (operationResponses . responsesResponses) .~ resps
      in pathItem verb op
