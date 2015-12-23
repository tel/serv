{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Serv.Internal.SServer where

import           Control.Monad.Trans
import           Data.Function                      ((&))
import           Data.Maybe                         (catMaybes)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Singletons
import           Data.Singletons.Prelude            hiding ((:>), Const)
import           Data.Singletons.TypeLits
import           Data.String
import           Data.Tagged
import           Data.Text                          (Text)
import qualified Network.HTTP.Types                 as HTTP
import qualified Network.Wai                        as Wai
import           Serv.Internal.Api
import           Serv.Internal.Api.Analysis
import qualified Serv.Internal.Header               as Header
import qualified Serv.Internal.Header.Serialization as HeaderS
import           Serv.Internal.Rec
import qualified Serv.Internal.Server.Context       as Ctx
import qualified Serv.Internal.Server.Error         as Error
import           Serv.Internal.Server.Monad
import           Serv.Internal.Cors as Cors
import           Serv.Internal.Server.Response
import           Serv.Internal.Server.Type
import           Serv.Internal.Verb

server :: forall m (api :: Api Symbol *) . Monad m => Sing api -> Impl m api -> Server m
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

    SEndpoint sAnn sHandlers -> Server $ do
      let verbs = augmentVerbs (inspectVerbs sHandlers)
      isTerminal <- pathIsEmpty
      if not isTerminal
        then runServer notFoundS
        else do
          mayVerb <- getVerb
          case mayVerb of
            Nothing -> runServer (methodNotAllowedS verbs)
            Just verb
              | verb == OPTIONS ->
                  -- TODO add CORS info
                  return
                    $ WaiResponse
                    $ Wai.responseLBS
                        HTTP.ok200
                        (catMaybes [HeaderS.headerPair Header.SAllow verbs])
                        ""

              | verb `Set.member` verbs -> do
                  -- TODO add CORS info
                  runServer (handles verbs sHandlers impl)

              -- Strictly this is unnecessary but it'll let us short-circuit
              -- method-not-found error detection
              | otherwise -> runServer (methodNotAllowedS verbs)

    sPath :%> sApi' -> Server $
      case sPath of
        SConst sym -> withKnownSymbol sym $ do
          maySeg <- takeSegment
          case maySeg of
            Nothing -> runServer notFoundS
            Just seg
              | seg /= fromString (symbolVal sym) -> runServer notFoundS
              | otherwise -> runServer (server sApi' impl)

        SWildcard -> do
          segs <- takeAllSegments
          runServer (server sApi' (impl segs))

        -- TODO
        --
        -- SHeaderAs sHdr sVal -> _sHeaderAs sHdr sVal
        --
        -- SSeg _sName _sTy -> _sSeg
        --
        -- SHeader sHdr _sTy -> _sHeader sHdr

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

handles :: Monad m => Set Verb -> Sing hs -> ImplEndpoint m hs -> Server m
handles verbs SNil MethodNotAllowed = methodNotAllowedS verbs
handles verbs (SCons sHandler sRest) (handler :<|> implRest) =
  handle sHandler handler
  `orElse`
  handles verbs sRest implRest

handle :: forall m (h :: Handler Symbol *) . Sing h -> ImplHandler m h -> Server m
handle sH impl =
  case sH of
    _ -> undefined

    -- TODO
    -- SMethod sVerb sHdrs sBody -> _handleBody sVerb sHdrs sBody impl
    -- SCaptureBody sCTypes sTy sNext ->
    --   handle sNext (impl $ _handleCapture sCTypes sTy)
    -- SCaptureHeaders sHdrs sNext ->
    --   handle sNext (impl $ _handleCaptureHdrs sHdrs)
    -- SCaptureQuery sQuery sNext ->
    --   handle sNext (impl $ _handleCaptureQuery sQuery)

-- Type Families
-- ----------------------------------------------------------------------------

type family Impl (m :: * -> *) (a :: Api Symbol *) :: * where
  Impl m Raw = m Wai.Application
  Impl m (p :> a) = ImplPath m p a
  Impl m (Endpoint ann hs) = ImplEndpoint m hs

  Impl m (OneOf '[]) = NotFound
  Impl m (OneOf (a ': as)) =
    Impl m a :<|> Impl m (OneOf as)

type family ImplEndpoint (m :: * -> *) (hs :: [Handler Symbol *]) :: * where
  ImplEndpoint m '[] = MethodNotAllowed
  ImplEndpoint m (h ': hs) =
    ImplHandler m h :<|> ImplEndpoint m hs

type family ImplOneOf (m :: * -> *) (as :: [Api Symbol *]) :: * where
  ImplOneOf m '[] = NotFound
  ImplOneOf m (a ': as) =
    Impl m a :<|> ImplOneOf m as

type family ImplPath (m :: * -> *)
                     (p :: Path Symbol *)
                     (a :: Api Symbol *) :: * where
  ImplPath m (Const s) next = Impl m next
  ImplPath m (HeaderAs s v) next = Impl m next
  ImplPath m (Seg sym a) next = Tagged sym a -> Impl m next
  ImplPath m (Header name a) next = a -> Impl m next
  ImplPath m Wildcard next = [Text] -> Impl m next
  ImplPath m (Cors ty) next = Impl m next

type family ImplHandler (m :: * -> *) (h :: Handler Symbol *) :: * where
  ImplHandler m (CaptureBody ctypes a h) = a -> ImplHandler m h
  ImplHandler m (CaptureHeaders hspec h) = Rec hspec -> ImplHandler m h
  ImplHandler m (CaptureQuery qspec h) = Rec qspec -> ImplHandler m h
  ImplHandler m (Method verb htypes body) = m (Response htypes body)
