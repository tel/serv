{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Serv.Internal.Server where

import qualified Data.ByteString.Lazy as Sl
import           Control.Monad.Trans
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
import           Serv.Internal.Rec
import           Serv.Internal.Server.Monad
import           Serv.Internal.Server.Response
import           Serv.Internal.MediaType
import           Serv.Internal.Server.Type
import qualified Serv.Internal.URI                  as URI
import           Serv.Internal.Verb

addCorsHeaders :: Maybe [HTTP.Header] -> ServerValue -> ServerValue
addCorsHeaders hdrs v =
  case v of
    WaiResponse resp ->
      WaiResponse (Wai.mapResponseHeaders (maybe [] id hdrs ++) resp)
    other -> other

server
  :: ((constr :=> impl) ~ I m api, Monad m, constr)
  => Sing api -> impl -> Server m
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
                  -- TODO add CORS info
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

-- | Augment the Set of allowed verbs by adding OPTIONS and, as necessary,
-- HEAD.
augmentVerbs :: Set Verb -> Set Verb
augmentVerbs = augHead . augOptions where
  augHead s
    | Set.member GET s = Set.insert HEAD s
    | otherwise = s
  augOptions = Set.insert OPTIONS

handles :: (CEndpoint hs, Monad m) => Set Verb -> Sing hs -> ImplEndpoint m hs -> Server m
handles verbs SNil MethodNotAllowed = methodNotAllowedS verbs
handles verbs (SCons sHandler sRest) (handler :<|> implRest) =
  handle sHandler handler
  `orElse`
  handles verbs sRest implRest

handle :: (CHandler h, Monad m) => Sing h -> ImplHandler m h -> Server m
handle sH impl = Server $
  case sH of
    SMethod sVerb _sHdrs sBody -> do
      mayVerb <- getVerb
      case (mayVerb, fromSing sVerb) of
        (Nothing, _) ->
          runServer notFoundS

        (Just HEAD, GET) -> do
          resp <- lift impl
          handleResponse SEmpty (deleteBody resp)

        (Just req, have)
          | req /= have ->
              runServer notFoundS
          | otherwise -> do
              resp <- lift impl
              handleResponse sBody resp

    -- TODO

    SCaptureBody _sCTypes _sTy _sH' ->
      undefined -- runServer (handle sH' (impl _))
    SCaptureHeaders _sHdrs _sH' ->
      undefined -- runServer (handle sH' (impl _))
    SCaptureQuery _sQ _sH' ->
      undefined -- runServer (handle sH' (impl _))

handleResponse
  :: (HeaderS.HeaderEncodes htypes, CBody b, Monad m)
  => Sing b -> Response htypes b -> InContext m ServerValue
handleResponse s resp =
  case (s, resp) of
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
        Just (_mt, body) ->
          return
            $ WaiResponse
            $ Wai.responseLBS
                status
                (secretHeaders ++ HeaderS.encodeHeaders headers)
                (Sl.fromStrict body)
    _ -> bugInGHC

-- negotiateContentAlways
--   :: AllEncoded a ctypes =>
--      Sing ctypes -> [Media.Quality MediaType] -> a -> Maybe (MediaType, S.ByteString)

    -- TODO
    -- SCaptureBody sCTypes sTy sNext ->
    --   handle sNext (impl $ _handleCapture sCTypes sTy)
    -- SCaptureHeaders sHdrs sNext ->
    --   handle sNext (impl $ _handleCaptureHdrs sHdrs)
    -- SCaptureQuery sQuery sNext ->
    --   handle sNext (impl $ _handleCaptureQuery sQuery)

-- encodeBody :: WaiResponse hdrs body => Context -> Response hdrs body -> ServerValue
-- encodeBody ctx resp =
--   case acceptHdr of
--     Left _ -> WaiResponse (waiResponse [] resp)
--     Right acceptList ->
--       WaiResponse (waiResponse acceptList resp)
--   where
--     (_, acceptHdr) = Context.examineHeader Hp.accept ctx
--
data (:=>) (c :: Constraint) (a :: *) where

type I m api = C api :=> Impl m api

-- Type Families
-- ----------------------------------------------------------------------------

type family Impl (m :: * -> *) (a :: Api Symbol *) :: * where
  Impl m Raw = m Wai.Application
  Impl m (OneOf apis) = ImplOneOf m apis
  Impl m (Endpoint ann hs) = ImplEndpoint m hs
  Impl m (p :> a) = ImplPath m p (Impl m a)

type family ImplEndpoint (m :: * -> *) (hs :: [Handler Symbol *]) :: * where
  ImplEndpoint m '[] = MethodNotAllowed
  ImplEndpoint m (h ': hs) =
    ImplHandler m h :<|> ImplEndpoint m hs

type family ImplOneOf (m :: * -> *) (as :: [Api Symbol *]) :: * where
  ImplOneOf m '[] = NotFound
  ImplOneOf m (a ': as) =
    Impl m a :<|> ImplOneOf m as

type family ImplPath (m :: * -> *) (p :: Path Symbol *) (r :: *) :: * where
  ImplPath m (Const s) next = next
  ImplPath m (HeaderAs s v) next = next
  ImplPath m (Seg sym a) next = Tagged sym a -> next
  ImplPath m (Header name a) next = a -> next
  ImplPath m Wildcard next = [Text] -> next
  ImplPath m (Cors ty) next = next

type family ImplHandler (m :: * -> *) (h :: Handler Symbol *) :: * where
  ImplHandler m (CaptureBody ctypes a h) = a -> ImplHandler m h
  ImplHandler m (CaptureHeaders hspec h) = Rec hspec -> ImplHandler m h
  ImplHandler m (CaptureQuery qspec h) = Rec qspec -> ImplHandler m h
  ImplHandler m (Method verb htypes body) = m (Response htypes body)

type family C (a :: Api Symbol *) :: Constraint where
  C Raw = ()
  C (Endpoint ann hs) = CEndpoint hs
  C (OneOf apis) = COneOf apis
  C (p :> a) = (CPath p, C a)

type family COneOf (as :: [Api Symbol *]) :: Constraint where
  COneOf '[] = ()
  COneOf (a ': as) = (C a, COneOf as)

type family CEndpoint (hs :: [Handler Symbol *]) :: Constraint where
  CEndpoint '[] = ()
  CEndpoint (h ': hs) = (CHandler h, CEndpoint hs)

type family CHandler (h :: Handler Symbol *) :: Constraint where
  CHandler (CaptureBody ctypes a h) = ((), CHandler h)
  CHandler (CaptureHeaders hspec h) = ((), CHandler h)
  CHandler (CaptureQuery qspec h) = ((), CHandler h)
  CHandler (Method verb htypes b) = (CBody b, HeaderS.HeaderEncodes htypes)

type family CBody (b :: Body *) :: Constraint where
  CBody Empty = ()
  CBody (HasBody ctypes a) = AllEncoded a ctypes

type family CPath (p :: Path Symbol *) :: Constraint where
  CPath (Const s) = KnownSymbol s
  CPath (HeaderAs s v) = (SingI s, KnownSymbol v)
  CPath (Seg sym a) = (KnownSymbol sym, URI.URIDecode a)
  CPath (Header name a) = HeaderS.HeaderDecode name a
  CPath Wildcard = ()
  CPath (Cors ty) = Cors.CorsPolicy ty
