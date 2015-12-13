{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.Server.Type where

import Data.String
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8         as S8
import qualified Data.ByteString.Lazy         as Sl
import           Data.Function                ((&))
import           Data.Proxy
import           Network.HTTP.Media           (MediaType, Quality, renderHeader)
import qualified Network.HTTP.Types           as HTTP
import qualified Network.Wai                  as Wai
import           Serv.Internal.Api
import qualified Serv.Internal.Header         as Header
import qualified Serv.Internal.MediaType      as MediaType
import           Serv.Internal.Pair
import           Serv.Internal.Rec
import qualified Serv.Internal.Verb as Verb
import           Serv.Internal.Server.Context (Context)
import qualified Serv.Internal.Server.Context as Context
import           Serv.Internal.Server.Error   (RoutingError)
import qualified Serv.Internal.Server.Error   as Error

-- | A server implementation which always results in a "Not Found" error. Used to
-- give semantics to "pathological" servers like @'OneOf '[]@ and @Endpoint '[]@.
--
-- These servers could be statically disallowed but (1) they have a semantic
-- sense as described by this type exactly and (2) to do so would require the
-- creation and management of either larger types or non-empty proofs which would
-- be burdensome to carry about.
data NotHere = NotHere

-- | Actual servers are implemented effectfully; this is a no-op server which
-- immediately returns Not Found and applies no effects.
noOp :: Applicative m => m NotHere
noOp = pure NotHere

-- | Either one thing or the other. In particular, often this is used when we are
-- describing either one server implementation or the other. Used to give
-- semantics to @'OneOf@ and @'Endpoint@.
data a :<|> b = a :<|> b

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

runServerWai
  :: Context
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> (Server IO -> IO Wai.ResponseReceived)
runServerWai context respond server = do
  val <- runServer server context
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
        let verbNames = map Verb.standardName verbs
            allowHeader = S8.intercalate "," verbNames
        Wai.responseLBS HTTP.methodNotAllowed405 [("Allow", allowHeader)] ""

    WaiResponse resp -> respond resp

    -- We forward the request (frozen) and the respond handler
    -- on to the internal application
    Application app -> app (Context.request context) respond

-- A server executing in a given monad. We construct these from 'Api'
-- descriptions and corresponding 'Impl' descriptions for said 'Api's.
-- Ultimately, a 'Server', or at least a 'Server IO', is destined to be
-- transformed into a Wai 'Wai.Appliation', but 'Server' tracks around more
-- information useful for interpretation and route finding.
newtype Server m = Server { runServer :: Context -> m ServerValue }

transformServer :: (forall x . m x -> n x) -> Server m -> Server n
transformServer phi (Server act) = Server (phi . act)

-- | 'Server's form a semigroup trying each 'Server' in order and receiving
-- the leftmost one which does not end in an ignorable error.
--
-- Or, with less technical jargon, @m `orElse` n@ acts like @m@ except in the
-- case where @m@ returns an 'Error.ignorable' 'Error.Error' in which case control
-- flows on to @n@.
orElse :: Monad m => Server m -> Server m -> Server m
orElse sa sb = Server $ \ctx -> do
  a <- runServer sa ctx
  case a of
    RoutingError e
      | Error.ignorable e -> runServer sb ctx
      | otherwise -> return a
    _ -> return a

routingError :: Monad m => RoutingError -> m ServerValue
routingError err = return (RoutingError err)

-- Responses
-- ----------------------------------------------------------------------------

-- | Responses generated in 'Server' implementations.
data Response (headers :: [Pair Header.HeaderName *]) body where
  Response :: HTTP.Status -> Rec headers -> a -> Response headers ('Body ctypes a)
  EmptyResponse :: HTTP.Status -> Rec headers -> Response headers 'Empty

basicResponse :: HTTP.Status -> Response '[] 'Empty
basicResponse status = EmptyResponse status Nil

withBody
  :: Proxy ctypes -> a
  -> Response headers 'Empty -> Response headers ('Body ctypes a)
withBody _ a (EmptyResponse status headers) =
  Response status headers a

withHeader
  :: Proxy name -> value
  -> Response headers body -> Response (name '::: value ': headers) body
withHeader proxy val r = case r of
  Response status headers body ->
    Response status (headers & proxy -: val) body
  EmptyResponse status headers ->
    EmptyResponse status (headers & proxy -: val)


-- Reflection
-- ----------------------------------------------------------------------------

class WaiResponse headers body where
  waiResponse :: [Quality MediaType] -> Response headers body -> Wai.Response

instance Header.ReflectHeaders headers => WaiResponse headers 'Empty where
  waiResponse _ (EmptyResponse status headers) =
    Wai.responseLBS status (Header.reflectHeaders headers) ""

instance
  (Header.ReflectHeaders headers, MediaType.ReflectEncoders ctypes a) =>
    WaiResponse headers ('Body ctypes a)
  where
    waiResponse accepts (Response status headers value) =
      case MediaType.negotiateContentAlways (Proxy :: Proxy ctypes) accepts value of
        Nothing -> Wai.responseLBS HTTP.notAcceptable406 [] ""
        Just (mtChosen, result) ->
          let headers0 = Header.reflectHeaders headers
              headers1 = ("Content-Type", renderHeader mtChosen) : headers0
          in Wai.responseLBS status headers1 $ Sl.fromStrict result