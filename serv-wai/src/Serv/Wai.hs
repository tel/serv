{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Build an "implementation" of a given 'Api'-kinded type (e.g. @'Impl'
-- api@) which describes all of the logic for your server and then convert
-- it into a 'Server' value and then an 'Application'.
module Serv.Wai (

  -- * Implement a 'Server'
    server
  , Server

  -- ** Server transformation

  -- | Typically you use 'server' to construct a value @'Server' M@ for
  -- some @M@ specific to your application, either a transformer stack or
  -- an abstract monad constrained by @mtl@-like typeclasses. If @M@ is not
  -- 'IO' then 'serverApplication' cannot be used to build an
  -- 'Application', so instead we must first transform @M@ using a "run"
  -- function applied to 'mapServer'.
  --
  -- For instance, if @M@ is @StateT St IO@ then
  --
  -- @
  --     flip evalStateT s0 :: StateT St IO a -> IO a
  -- @
  --
  -- is a suitable "run" function we could apply
  -- using 'mapServer' to transform @'Server' M@ into @'Server' 'IO'@.

  , mapServer

  -- ** Execute it as an 'Application'
  , serverApplication
  , serverApplication'
  , serverApplication''

  -- * Constraints and Implementations

  -- | In order to call 'server' we must ensure that our @api :: 'Api'@
  -- type is decorated with the appropriate constraints and that the
  -- @'Impl' api@ type properly matches the 'Api'. This is achieved by
  -- analyzing the types with type-level functions, e.g. the closed type
  -- families 'Impl' and 'Constrain'.
  --
  -- NOTE: Closed type families are rather finnicky as to when they
  -- actually evaluate, so the factoring of these type families into
  -- smaller pieces is done by some trial and error.

  , Impl
  , Constrain

  -- ** Detailed constraints and implementations
  , AllImpl
  , AllHandlers
  , ImplHandler

  , ConstrainEndpoint
  , ConstrainHandler
  , ConstrainOutputs
  , ConstrainRespond
  , ConstrainBody

) where

import           Control.Monad.Trans
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as Sl
import           Data.CaseInsensitive          (CI)
import           Data.Maybe                    (catMaybes)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Tuple
import           Data.Singletons.TypeLits
import           Data.Text                     (Text)
import           GHC.Exts
import           Network.HTTP.Kinder.Header    (AllHeaderDecodes,
                                                AllHeaderEncodes,
                                                HeaderDecode (..), HeaderName, Sing (SAccept, SAllow, SContentType),
                                                headerEncodePair)
import           Network.HTTP.Kinder.MediaType (AllMimeDecode, AllMimeEncode,
                                                negotiatedMimeEncode)
import           Network.HTTP.Kinder.Query     (AllQueryDecodes)
import           Network.HTTP.Kinder.Status    (Status)
import qualified Network.HTTP.Kinder.Status    as St
import           Network.HTTP.Kinder.URI       (URIDecode (..))
import           Network.HTTP.Kinder.Verb      (Verb (..))
import           Network.Wai
import           Serv.Api
import           Serv.Wai.Analysis
import           Serv.Wai.Corec
import           Serv.Wai.Rec
import           Serv.Wai.Response
import           Serv.Wai.Type

type family Impl (m :: * -> *) api where
  Impl m Abstract = m (Context -> Application)

  Impl m (OneOf apis) = HList (AllImpl m apis)
  Impl m (Endpoint ann hs) = FieldRec (AllHandlers m hs)

  Impl m (Const s :> api) = Impl m api
  Impl m (HeaderAs s v :> api) = Impl m api
  Impl m (Seg s a :> api) = a -> Impl m api
  Impl m (Header n a :> api) = a -> Impl m api
  Impl m (Wildcard :> api) = [Text] -> Impl m api

type family AllImpl m apis where
  AllImpl m '[] = '[]
  AllImpl m (api ': apis) = Impl m api ': AllImpl m apis

type family AllHandlers m (hs :: [(Verb, Handler *)]) where
  AllHandlers m '[] = '[]
  AllHandlers m ( '(v, h) ': hs) =
    '(v, ImplHandler m h) ': AllHandlers m hs

type family ImplHandler m h where
  ImplHandler m (CaptureBody ts a h) = a -> ImplHandler m h
  ImplHandler m (CaptureHeaders hs h) = FieldRec hs -> ImplHandler m h
  ImplHandler m (CaptureQuery qs h) = FieldRec qs -> ImplHandler m h
  ImplHandler m (Outputs os) = m (SomeResponse os)

type family Constrain a :: Constraint where
  Constrain Abstract = ()

  Constrain (Endpoint ann hs) = ConstrainEndpoint hs

  Constrain (OneOf '[]) = ()
  Constrain (OneOf (api ': apis)) =
    (Constrain api, Constrain (OneOf apis))


  Constrain (Const s :> api) = Constrain api
  Constrain (HeaderAs s v :> api) = Constrain api
  Constrain (Seg s a :> api) = (Constrain api, URIDecode a)
  Constrain (Header n a :> api) = (Constrain api, HeaderDecode n a)
  Constrain (Wildcard :> api) = Constrain api

type family ConstrainEndpoint (hs :: [(Verb, Handler *)]) :: Constraint where
  ConstrainEndpoint '[] = ()
  ConstrainEndpoint ( '(v, h) ': hs) =
    (ConstrainHandler h, ConstrainEndpoint hs)

type family ConstrainHandler h :: Constraint where
  ConstrainHandler (Outputs os) =
    ConstrainOutputs os
  ConstrainHandler (CaptureBody ctypes a h) =
    (AllMimeDecode a ctypes, ConstrainHandler h)
  ConstrainHandler (CaptureHeaders hs h) =
    (AllHeaderDecodes hs, ConstrainHandler h)
  ConstrainHandler (CaptureQuery qs h) =
    (AllQueryDecodes qs, ConstrainHandler h)

type family ConstrainOutputs (os :: [(Status, Output *)]) :: Constraint where
  ConstrainOutputs '[] = ()
  ConstrainOutputs ((s ::: r) ': os) = (ConstrainRespond r, ConstrainOutputs os)

type family ConstrainRespond r :: Constraint where
  ConstrainRespond (Respond hs b) = (AllHeaderEncodes hs, ConstrainBody b)

type family ConstrainBody b :: Constraint where
  ConstrainBody Empty = ()
  ConstrainBody (HasBody ts a) = AllMimeEncode a ts

-- | Construct a 'Server' value from an @'Impl' api@ implementation
-- matching the @'Sing' api@ singleton. This is the primary function for
-- the entire package.
server :: (Constrain api, Monad m) => Sing api -> Impl m api -> Server m
server SAbstract mApp = returnServer (fmap Application mApp)
server (SOneOf SNil) RNil = notFound
server (SOneOf (SCons api apis)) (Identity impl :& impls) =
  server api impl `orElse` server (SOneOf apis) impls
server (path :%> api) impl =
  Server $ case path of
    SConst sym -> withKnownSymbol sym $ do
      maySeg <- popSegment
      runServer $ case maySeg of
        Nothing -> notFound
        Just seg
          | seg /= fromString (symbolVal sym) -> notFound
          | otherwise -> server api impl
    SWildcard -> do
      segs <- popAllSegments
      runServer (server api (impl segs))
    SHeaderAs h sExp -> do
      let expected = fromString (withKnownSymbol sExp (symbolVal sExp))
      ok <- expectHeader h expected
      runServer $ if ok
        then server api impl
        else notFound
    SSeg _name _ty -> do
      trySeg <- popSegment
      case trySeg of
        Nothing -> runServer notFound
        Just seg ->
          case uriDecode seg of
            Left err -> do
              setError . mkBadRequest . Just $ err
              runServer notFound
            Right val -> runServer $ server api (impl val)
    SHeader hdr _ty -> do
      tryVal <- getHeader hdr
      runServer $ case tryVal of
        Left err -> badRequest (Just err)
        Right val -> server api (impl val)
server (SEndpoint _ann handlers) impls = Server $ do
  let verbs = augmentVerbs (inspectVerbs handlers)
  isTerminal <- endOfPath
  if not isTerminal
    then runServer notFound
    else do
      mayVerb <- getVerb
      case mayVerb of
        Nothing -> runServer (methodNotAllowed verbs)
        Just verb
          | verb == OPTIONS -> do
            return $
              WaiResponse
              $ responseLBS
                  (St.httpStatus St.SOk)
                  (catMaybes [headerEncodePair SAllow verbs])
                  ""
          | verb `Set.member` verbs -> do
              runServer (handles verbs handlers impls)
          | otherwise -> runServer (methodNotAllowed verbs)

handles
  :: (ConstrainEndpoint hs, Monad m)
  => Set Verb -> Sing hs -> FieldRec (AllHandlers m hs) -> Server m
handles verbs SNil RNil = methodNotAllowed verbs
handles verbs (SCons (STuple2 sVerb sHandler) sRest) (ElField _verb handler :& implRest) =
  handleVerb sVerb sHandler handler
  `orElse`
  handles verbs sRest implRest

handleVerb :: forall h m (v :: Verb) . (ConstrainHandler h, Monad m) => Sing v -> Sing h -> ImplHandler m h -> Server m
handleVerb sVerb sH impl = Server $ do
  mayVerb <- getVerb
  case mayVerb of
    Nothing -> runServer notFound
    Just verbRequested
      | verbRequested == HEAD -> do
          runServer (handleHandler False sH impl)
      | verbRequested == fromSing sVerb -> do
          runServer (handleHandler True sH impl)
      | otherwise ->
          -- not methodNotAllowedS because we can't
          -- make that judgement locally.
          runServer notFound

handleHandler :: (ConstrainHandler h, Monad m) => Bool -> Sing h -> ImplHandler m h -> Server m
handleHandler presentBody sH impl = Server $
  case sH of
    SOutputs sAlts -> do
      someResponse <- lift impl
      handleResponse presentBody sAlts someResponse

    SCaptureHeaders sHdrs sH' -> do
      tryHdrs <- extractHeaders sHdrs
      case tryHdrs of
        Left errors ->
          runServer (badRequest (Just (unlines ("invalid headers:" : errors))))
        Right rec ->
          runServer (handleHandler presentBody sH' (impl rec))

    SCaptureQuery sQ sH' -> do
      tryQ <- extractQueries sQ
      case tryQ of
        Left errors ->
          runServer (badRequest (Just (unlines ("invalid query:" : errors))))
        Right rec ->
          runServer (handleHandler presentBody sH' (impl rec))

    SCaptureBody sCTypes _sTy sH' -> do
      eitval <- getBody sCTypes
      case eitval of
        Left err ->
          runServer (badRequest (Just ("bad body encoding: " ++ err)))
        Right val ->
          runServer (handleHandler presentBody sH' (impl val))


extractHeaders
  :: forall m (hs :: [(HeaderName, *)])
  . (AllHeaderDecodes hs, Monad m, Contextual m)
  => Sing hs -> m (Either [String] (FieldRec hs))
extractHeaders SNil = return (Right RNil)
extractHeaders (SCons (STuple2 hdr (_ty :: Sing a)) rest) = do
  tryRec <- extractHeaders rest
  tryHeader <- getHeader hdr
  return $ case (tryRec, tryHeader :: Either String a) of
    (Left errs, Left err) -> Left (err : errs)
    (Left errs, Right _) -> Left errs
    (Right _, Left err) -> Left [err]
    (Right rec, Right val) -> Right (ElField hdr val :& rec)

extractQueries
  :: forall m (qs :: [(Symbol, *)])
  . (AllQueryDecodes qs, Monad m, Contextual m)
  => Sing qs -> m (Either [String] (FieldRec qs))
extractQueries SNil = return (Right RNil)
extractQueries (SCons (STuple2 qsym (_ty :: Sing a)) rest) = do
  tryRec <- extractQueries rest
  tryQuery <- getQuery qsym
  return $ case (tryRec, tryQuery :: Either String a) of
    (Left errs, Left err) -> Left (err : errs)
    (Left errs, Right _) -> Left errs
    (Right _, Left err) -> Left [err]
    (Right rec, Right val) -> Right (ElField qsym val :& rec)

handleResponse
  :: (ConstrainOutputs alts, Monad m, Contextual m)
  => Bool -> Sing alts -> SomeResponse alts -> m ServerResult

handleResponse includeBody (SCons _ sRest) (Skip someResponse) =
  handleResponse includeBody sRest someResponse

handleResponse
  includeBody
  (SCons (STuple2 sStatus (SRespond _sHeaders sBody)) _)
  (Stop resp) =

  case (sBody, resp) of
    (SEmpty, EmptyResponse secretHeaders headers) ->
      return
        $ WaiResponse
        $ responseLBS
            (St.httpStatus sStatus)
            (secretHeaders ++ encodeHeaders headers)
            ""
    (SHasBody sCtypes _sTy, ContentResponse secretHeaders headers a)
      | not includeBody -> do
          return
            $ WaiResponse
            $ responseLBS
                (St.httpStatus sStatus)
                (secretHeaders ++ encodeHeaders headers)
                ""
      | otherwise -> do
        eitAccept <- getHeader SAccept
        let accepts = either (const []) id eitAccept
        case negotiatedMimeEncode sCtypes of
          Nothing ->
            return
              $ WaiResponse
              $ responseLBS (St.httpStatus St.SNotAcceptable) [] ""
          Just nego -> do
            let (mt, body) = nego accepts a
                newHeaders = catMaybes [ headerEncodePair SContentType mt ]
            return
              $ WaiResponse
              $ responseLBS
                  (St.httpStatus sStatus)
                  ( newHeaders
                    ++ secretHeaders
                    ++ encodeHeaders headers
                  )
                  (Sl.fromStrict body)

handleResponse _ _ _ = error "BUG IN GHC"

-- | Augment the Set of allowed verbs by adding OPTIONS and, as necessary,
-- HEAD.
augmentVerbs :: Set Verb -> Set Verb
augmentVerbs = augHead . augOptions where
  augHead s
    | Set.member GET s = Set.insert HEAD s
    | otherwise = s
  augOptions = Set.insert OPTIONS

encodeHeaders :: AllHeaderEncodes rs => FieldRec rs -> [(CI S.ByteString, S.ByteString)]
encodeHeaders = catMaybes . encodeHeaders'

-- | Convert a record of headers into a raw bytes format
encodeHeaders' :: AllHeaderEncodes rs => FieldRec rs -> [Maybe (CI S.ByteString, S.ByteString)]
encodeHeaders' rec =
  case rec of
    RNil -> []
    ElField s val :& rest ->
      headerEncodePair s val : encodeHeaders' rest
