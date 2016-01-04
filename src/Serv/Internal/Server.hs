{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Serv.Internal.Server where

import           Control.Monad.Trans
import qualified Data.ByteString.Lazy               as Sl
import           Data.Maybe                         (catMaybes)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Singletons
import           Data.Singletons.Prelude            hiding ((:>), Const)
import           Data.Singletons.TypeLits
import           Data.String
import           Data.Tagged
import           Data.Text                          (Text)
import           GHC.Exts
import qualified Network.HTTP.Types                 as HTTP
import qualified Network.Wai                        as Wai
import           Serv.Internal.Api
import           Serv.Internal.Api.Analysis
import           Serv.Internal.Cors                 as Cors
import qualified Serv.Internal.Header               as Header
import qualified Serv.Internal.Header.Serialization as HeaderS
import           Serv.Internal.MediaType
import           Serv.Internal.Rec
import           Serv.Internal.Server.Monad
import           Serv.Internal.Server.Response
import           Serv.Internal.Server.Type
import qualified Serv.Internal.URI                  as URI
import           Serv.Internal.Verb

-- | Construct a 'Server' value for a given 'Api' type by providing an
-- 'Impl' describing the semantics and server behaviors for that 'Api'.
--
-- The type families 'Impl' and 'Constrain' are important to understand.
-- 'Constrain' describes what typeclasses must be instantiated for various
-- component types within an 'Api' in order to have sufficient information
-- to construct a 'Server'. The 'Impl' type family defines what types
-- describe behaviors befitting the given 'Api' type.
--
-- For instance, for an 'Api' like
--
-- @
--   type A = Endpoint () '[ Method GET '[ CacheControl :: RawText ] Empty ]
-- @
-- @
--
-- 'Constrain' A@ forces us to have a 'HeaderS.HeaderEncode' instance for
-- 'CacheControl' and 'RawText' and @'Impl' A@ tells us that to implement
-- this server we must provide a 'Response' type containing no body and
-- a header for 'CacheControl' taking type 'RawText'.
--
server :: (Constrain api, Monad m) => Sing api -> Impl m api -> Server m
server sApi impl =
  case sApi of
    SRaw -> Server $ lift (fmap Application impl)

    SOneOf sChoices ->
      case (sChoices, impl) of
        (SNil, NotFound) -> notFoundS
        (SCons sApi' sRest, implNow :<|> implLater) ->
          server sApi' implNow
          `orElse`
          server (SOneOf sRest) implLater

    SEndpoint _sAnn sHandlers -> Server $ do
      let verbs = augmentVerbs (inspectVerbs sHandlers)
      isTerminal <- pathIsEmpty
      if not isTerminal
        then runServer notFoundS
        else do
          mayVerb <- getVerb
          case mayVerb of
            Nothing -> runServer (methodNotAllowedS verbs)
            Just verb
              | verb == OPTIONS -> do
                  corsHs <- corsHeaders sHandlers Cors.IncludeMethods
                  let value =
                          WaiResponse
                          $ Wai.responseLBS
                              HTTP.ok200
                              (catMaybes [HeaderS.headerPair Header.SAllow verbs])
                              ""
                  return (addCorsHeaders corsHs value)

              | verb `Set.member` verbs -> do
                  corsHs <- corsHeaders sHandlers Cors.Don'tIncludeMethods
                  value <- runServer (handles verbs sHandlers impl)
                  return (addCorsHeaders corsHs value)

              -- Strictly this is unnecessary but it'll let us short-circuit
              -- method-not-found error detection
              | otherwise -> runServer (methodNotAllowedS verbs)

    sPath :%> sApi' -> Server $
      case sPath of
        SConst sym -> withKnownSymbol sym $ do
          maySeg <- takeSegment
          runServer $ case maySeg of
            Nothing -> notFoundS
            Just seg
              | seg /= fromString (symbolVal sym) -> notFoundS
              | otherwise -> server sApi' impl

        SWildcard -> do
          segs <- takeAllSegments
          runServer (server sApi' (impl segs))

        SHeaderAs sHdr sVal -> do
          ok <- expectHeader sHdr (fromString (symbolVal sVal))
          runServer $ if ok
            then server sApi' impl
            else notFoundS

        SSeg _sName _sTy -> do
          trySeg <- takeSegment
          runServer $ case trySeg of
            Nothing -> notFoundS
            Just seg ->
              case URI.uriDecode seg of
                Left err -> badRequestS (Just err)
                Right val -> server sApi' (impl (Tagged val))

        SHeader sHdr (_sTy :: Sing a) -> do
          tryVal <- examineHeader sHdr
          runServer $ case tryVal of
            Left err -> badRequestS (Just err)
            Right val -> server sApi' (impl (val :: a))

        SCors sTy -> do
          addCorsPolicy (Cors.corsPolicy sTy)
          runServer (server sApi' impl)

addCorsHeaders :: Maybe [HTTP.Header] -> ServerValue -> ServerValue
addCorsHeaders hdrs v =
  case v of
    WaiResponse resp ->
      WaiResponse (Wai.mapResponseHeaders (maybe [] id hdrs ++) resp)
    other -> other

-- | Augment the Set of allowed verbs by adding OPTIONS and, as necessary,
-- HEAD.
augmentVerbs :: Set Verb -> Set Verb
augmentVerbs = augHead . augOptions where
  augHead s
    | Set.member GET s = Set.insert HEAD s
    | otherwise = s
  augOptions = Set.insert OPTIONS

handles :: (Constrain_Endpoint hs, Monad m) => Set Verb -> Sing hs -> Impl_Endpoint m hs -> Server m
handles verbs SNil MethodNotAllowed = methodNotAllowedS verbs
handles verbs (SCons sHandler sRest) (handler :<|> implRest) =
  handle sHandler handler
  `orElse`
  handles verbs sRest implRest

handle :: (Constrain_Handler h, Monad m) => Sing h -> Impl_Handler m h -> Server m
handle sH impl = Server $
  case sH of
    SMethod sVerb _sHdrs sBody -> do
      mayVerb <- getVerb
      case (mayVerb, fromSing sVerb) of
        (Nothing, _) ->
          runServer notFoundS

        (Just HEAD, GET) -> do
          AResponse resp <- lift impl
          handleResponse SEmpty (deleteBody resp)

        (Just req, have)
          | req /= have ->
              runServer notFoundS
          | otherwise -> do
              AResponse resp <- lift impl
              handleResponse sBody resp

    -- TODO: These...

    SCaptureBody _sCTypes _sTy _sH' ->
      undefined -- runServer (handle sH' (impl _))
    SCaptureHeaders _sHdrs _sH' ->
      undefined -- runServer (handle sH' (impl _))
    SCaptureQuery _sQ _sH' ->
      undefined -- runServer (handle sH' (impl _))

handleResponse
  :: (HeaderS.HeaderEncodes htypes, Constrain_Body b, Monad m)
  => Sing b -> Response e htypes b -> InContext m ServerValue
handleResponse s resp =
  case (s, resp) of
    (_, ErrorResponse status headers body) ->
      return
        $ WaiResponse
        $ Wai.responseLBS
            status
            headers
            (maybe "" id body)
    (SEmpty, EmptyResponse status secretHeaders headers) ->
      return
        $ WaiResponse
        $ Wai.responseLBS
            status
            (secretHeaders ++ HeaderS.encodeHeaders headers)
            ""
    (SHasBody sCtypes _sTy, Response status secretHeaders headers a) -> do
      accepts <- examineHeader Header.SAccept
      case negotiateContentAlways sCtypes (either (const []) id accepts) a of
        Nothing ->
          return
            $ WaiResponse
            $ Wai.responseLBS HTTP.notAcceptable406 [] ""
        Just (mt, body) -> do
          let newHeaders =
                catMaybes [ HeaderS.headerPair Header.SContentType mt ]
          return
            $ WaiResponse
            $ Wai.responseLBS
                status
                ( newHeaders
                  ++ secretHeaders
                  ++ HeaderS.encodeHeaders headers
                )
                (Sl.fromStrict body)
    _ -> bugInGHC

-- Type Families
-- ----------------------------------------------------------------------------

-- | Given a monad @M@ and an 'Api' type @A@ the type @Impl M A@ contains
-- values describing "server implementations". These types describe the
-- custom behaviors of the server within each capacity outlined by the
-- `Api` type @A@.
--
-- Beyond 'Impl' there are also four constituent type families; 3 describe
-- the implementation types for each `Api` constructor and the last
-- describes the specific implmentation type for the 'Handler' kind.
type family Impl (m :: * -> *) (a :: Api Symbol *) :: * where
  Impl m Raw = m Wai.Application
  Impl m (OneOf apis) = Impl_OneOf m apis
  Impl m (Endpoint ann hs) = Impl_Endpoint m hs
  Impl m (p :> a) = Impl_Path m p (Impl m a)

type family Impl_Endpoint (m :: * -> *) (hs :: [Handler Symbol *]) :: * where
  Impl_Endpoint m '[] = MethodNotAllowed
  Impl_Endpoint m (h ': hs) =
    Impl_Handler m h :<|> Impl_Endpoint m hs

type family Impl_OneOf (m :: * -> *) (as :: [Api Symbol *]) :: * where
  Impl_OneOf m '[] = NotFound
  Impl_OneOf m (a ': as) =
    Impl m a :<|> Impl_OneOf m as

type family Impl_Path (m :: * -> *) (p :: Path Symbol *) (r :: *) :: * where
  Impl_Path m (Const s) next = next
  Impl_Path m (HeaderAs s v) next = next
  Impl_Path m (Seg sym a) next = Tagged sym a -> next
  Impl_Path m (Header name a) next = a -> next
  Impl_Path m Wildcard next = [Text] -> next
  Impl_Path m (Cors ty) next = next

type family Impl_Handler (m :: * -> *) (h :: Handler Symbol *) :: * where
  Impl_Handler m (CaptureBody ctypes a h) = a -> Impl_Handler m h
  Impl_Handler m (CaptureHeaders hspec h) = Rec hspec -> Impl_Handler m h
  Impl_Handler m (CaptureQuery qspec h) = Rec qspec -> Impl_Handler m h
  Impl_Handler m (Method verb htypes body) = m (AResponse htypes body)

type family Constrain (a :: Api Symbol *) :: Constraint where
  Constrain Raw = ()
  Constrain (Endpoint ann hs) = Constrain_Endpoint hs
  Constrain (OneOf apis) = Constrain_OneOf apis
  Constrain (p :> a) = (Constrain_Path p, Constrain a)

type family Constrain_OneOf (as :: [Api Symbol *]) :: Constraint where
  Constrain_OneOf '[] = ()
  Constrain_OneOf (a ': as) = (Constrain a, Constrain_OneOf as)

type family Constrain_Endpoint (hs :: [Handler Symbol *]) :: Constraint where
  Constrain_Endpoint '[] = ()
  Constrain_Endpoint (h ': hs) = (Constrain_Handler h, Constrain_Endpoint hs)

type family Constrain_Handler (h :: Handler Symbol *) :: Constraint where
  Constrain_Handler (CaptureBody ctypes a h) = ((), Constrain_Handler h)
  Constrain_Handler (CaptureHeaders hspec h) = ((), Constrain_Handler h)
  Constrain_Handler (CaptureQuery qspec h) = ((), Constrain_Handler h)
  Constrain_Handler (Method verb htypes b) = (Constrain_Body b, HeaderS.HeaderEncodes htypes)

type family Constrain_Body (b :: Body *) :: Constraint where
  Constrain_Body Empty = ()
  Constrain_Body (HasBody ctypes a) = AllEncoded a ctypes

type family Constrain_Path (p :: Path Symbol *) :: Constraint where
  Constrain_Path (Const s) = KnownSymbol s
  Constrain_Path (HeaderAs s v) = (SingI s, KnownSymbol v)
  Constrain_Path (Seg sym a) = (KnownSymbol sym, URI.URIDecode a)
  Constrain_Path (Header name a) = HeaderS.HeaderDecode name a
  Constrain_Path Wildcard = ()
  Constrain_Path (Cors ty) = Cors.CorsPolicy ty
