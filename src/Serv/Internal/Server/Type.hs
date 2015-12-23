{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.Server.Type where

import qualified Data.ByteString.Char8              as S8
import qualified Data.ByteString.Lazy               as Sl
import           Data.Function                      ((&))
import           Data.Maybe                         (catMaybes)
import           Data.Maybe                         (fromMaybe)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Singletons
import           Data.String
import           GHC.TypeLits
import           Network.HTTP.Media                 (MediaType, Quality,
                                                     renderHeader)
import qualified Network.HTTP.Types                 as HTTP
import qualified Network.Wai                        as Wai
import           Serv.Internal.Api
import qualified Serv.Internal.Header               as Header
import qualified Serv.Internal.Header.Serialization as HeaderS
import qualified Serv.Internal.MediaType            as MediaType
import           Serv.Internal.Pair
import           Serv.Internal.Rec
import           Serv.Internal.Server.Context       (Context)
import qualified Serv.Internal.Server.Context       as Context
import           Serv.Internal.Server.Error         (RoutingError)
import qualified Serv.Internal.Server.Error         as Error
import qualified Serv.Internal.Verb                 as Verb
import Serv.Internal.Server.Monad

-- | A server implementation which always results in a "Not Found" error. Used to
-- give semantics to "terminal" server @'OneOf '[]@.
--
-- These servers could be statically disallowed but (1) they have a semantic
-- sense as described by this type exactly and (2) to do so would require the
-- creation and management of either larger types or non-empty proofs which would
-- be burdensome to carry about.
data NotFound = NotFound

-- | A server implementation which always results in a "Method Not Allowed" error. Used to
-- give semantics to the "terminal" server @Endpoint '[]@.
--
-- These servers could be statically disallowed but (1) they have a semantic
-- sense as described by this type exactly and (2) to do so would require the
-- creation and management of either larger types or non-empty proofs which would
-- be burdensome to carry about.
data MethodNotAllowed = MethodNotAllowed

-- | Either one thing or the other. In particular, often this is used when we are
-- describing either one server implementation or the other. Used to give
-- semantics to @'OneOf@ and @'Endpoint@.
data a :<|> b = a :<|> b

infixr 5 :<|>

-- | A return value from a 'Server' computation.
data ServerValue
  = RoutingError RoutingError
    -- ^ Routing errors arise when a routing attempt fails and, depending on the
    -- error, either we should recover and backtrack or resolve the entire response
    -- with that error.
  | WaiResponse Wai.Response
    -- ^ If the response is arising from the 'Server' computation itself it will
    -- be transformed automatically into a 'Wai.Response' value we can handle
    -- directly. These are opaque to routing, assumed successes.
  | Application Wai.Application
    -- ^ If the application demands an "upgrade" or ties into another server
    -- mechanism then routing at that location will return the (opaque)
    -- 'Application' to continue handling.

-- A server executing in a given monad. We construct these from 'Api'
-- descriptions and corresponding 'Impl' descriptions for said 'Api's.
-- Ultimately, a 'Server', or at least a 'Server IO', is destined to be
-- transformed into a Wai 'Wai.Appliation', but 'Server' tracks around more
-- information useful for interpretation and route finding.
newtype Server m = Server { runServer :: InContext m ServerValue }

-- Lift an effect transformation on to a Server
transformServer :: (forall x . m x -> n x) -> Server m -> Server n
transformServer phi (Server act) = Server (mapInContext phi act)

-- | 'Server's form a semigroup trying each 'Server' in order and receiving
-- the leftmost one which does not end in an ignorable error.
--
-- Or, with less technical jargon, @m `orElse` n@ acts like @m@ except in the
-- case where @m@ returns an 'Error.ignorable' 'Error.Error' in which case control
-- flows on to @n@.
orElse :: Monad m => Server m -> Server m -> Server m
orElse sa sb = Server $ do
  (ctx, a) <- fork (runServer sa)
  case a of
    RoutingError e
      | Error.ignorable e -> runServer sb
      | otherwise -> restore (ctx, a)
    _ -> restore (ctx, a)

-- | Server which immediately returns 'Error.NotFound'
notFoundS :: Monad m => Server m
notFoundS = Server $ routingError Error.NotFound

methodNotAllowedS :: Monad m => Set Verb.Verb -> Server m
methodNotAllowedS vs = Server $ routingError (Error.MethodNotAllowed vs)

routingError :: Monad m => RoutingError -> m ServerValue
routingError err = return (RoutingError err)

-- Interpretation
-- ----------------------------------------------------------------------------

runServerWai
  :: Context
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> (Server IO -> IO Wai.ResponseReceived)
runServerWai context respond server = do
  val <- runInContext (runServer server) context
  case val of
    RoutingError err -> respond $ case err of
      Error.NotFound ->
        Wai.responseLBS HTTP.notFound404 [] ""
      Error.BadRequest e -> do
        let errString = fromString (fromMaybe "" e)
        Wai.responseLBS HTTP.badRequest400 [] (fromString errString)
      Error.UnsupportedMediaType ->
        Wai.responseLBS HTTP.unsupportedMediaType415 [] ""
      Error.MethodNotAllowed verbs -> do
        Wai.responseLBS
          HTTP.methodNotAllowed405
          (catMaybes [HeaderS.headerPair Header.SAllow verbs])
          ""

    WaiResponse resp -> respond resp

    -- We forward the request (frozen) and the respond handler
    -- on to the internal application
    Application app -> app (Context.request context) respond

