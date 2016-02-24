{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Serv.Wai where

import           Control.Monad.Trans
import qualified Data.ByteString.Lazy          as Sl
import qualified Data.ByteString          as S
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
import           Network.HTTP.Kinder.MediaType (AllMimeEncode,
                                                negotiatedMimeEncode)
import           Network.HTTP.Kinder.Query     (AllQueryDecodes)
import           Network.HTTP.Kinder.Status    (Status)
import qualified Network.HTTP.Kinder.Status    as St
import           Network.HTTP.Kinder.URI       (URIDecode (..))
import           Network.HTTP.Kinder.Verb      (Verb (..))
import           Network.Wai                   (Application)
import           Network.Wai
import           Serv.Api
import           Serv.Api.Analysis
import           Serv.Wai.Corec
import           Serv.Wai.Rec
import           Serv.Wai.Response
import           Serv.Wai.Response             (SomeResponse)
import           Serv.Wai.Type

type family Impl (m :: * -> *) api where
  Impl m Abstract = m (Context -> Application)

  Impl m (OneOf apis) = HList (AllImpl m apis)
  Impl m (Endpoint ann hs) = HList (AllHandlers m hs)

  Impl m (Const s :> api) = Impl m api
  Impl m (HeaderAs s v :> api) = Impl m api
  Impl m (Seg s a :> api) = a -> Impl m api
  Impl m (Header n a :> api) = a -> Impl m api
  Impl m (Wildcard :> api) = [Text] -> Impl m api

type family AllImpl m apis where
  AllImpl m '[] = '[]
  AllImpl m (api ': apis) = Impl m api ': AllImpl m apis

type family AllHandlers m hs where
  AllHandlers m '[] = '[]
  AllHandlers m (h ': hs) = ImplHandler m h ': AllHandlers m hs

type family ImplHandler m h where
  ImplHandler m (CaptureBody ts a h) = a -> ImplHandler m h
  ImplHandler m (CaptureHeaders hs h) = FieldRec hs -> ImplHandler m h
  ImplHandler m (CaptureQuery qs h) = FieldRec qs -> ImplHandler m h
  ImplHandler m (Method v os) = m (SomeResponse os)

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

type family ConstrainEndpoint hs :: Constraint where
  ConstrainEndpoint '[] = ()
  ConstrainEndpoint (h ': hs) =
    (ConstrainHandler h, ConstrainEndpoint hs)

type family ConstrainHandler h :: Constraint where
  ConstrainHandler (Method verb os) =
    ConstrainOutputs os
  ConstrainHandler (CaptureBody ctypes a h) =
    ConstrainHandler h -- TODO
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
      runServer $ case trySeg of
        Nothing -> notFound
        Just seg ->
          case uriDecode seg of
            Left err -> badRequest (Just err)
            Right val -> server api (impl val)
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

handles :: (ConstrainEndpoint hs, Monad m) => Set Verb -> Sing hs -> HList (AllHandlers m hs) -> Server m
handles verbs SNil RNil = methodNotAllowed verbs
handles verbs (SCons sHandler sRest) (Identity handler :& implRest) =
  handle sHandler handler
  `orElse`
  handles verbs sRest implRest

handle :: (ConstrainHandler h, Monad m) => Sing h -> ImplHandler m h -> Server m
handle sH impl = Server $
  case sH of
    SMethod sVerb sAlts -> do
      mayVerb <- getVerb
      let verbProvided = fromSing sVerb
      case mayVerb of
        Nothing -> runServer notFound
        Just verbRequested
          | verbRequested == HEAD -> do
              someResponse <- lift impl
              handleResponse False sAlts someResponse
          | verbRequested == verbProvided -> do
              someResponse <- lift impl
              handleResponse True sAlts someResponse
          | otherwise ->
              runServer notFound -- not methodNotAllowedS because we can't
                                 -- make that judgement locally.

    SCaptureHeaders sHdrs sH' -> do
      tryHdrs <- extractHeaders sHdrs
      case tryHdrs of
        Left errors ->
          runServer (badRequest (Just (unlines ("invalid headers:" : errors))))
        Right rec ->
          runServer (handle sH' (impl rec))

    SCaptureQuery sQ sH' -> do
      tryQ <- extractQueries sQ
      case tryQ of
        Left errors ->
          runServer (badRequest (Just (unlines ("invalid query:" : errors))))
        Right rec ->
          runServer (handle sH' (impl rec))

    -- TODO: These...

    SCaptureBody _sCTypes _sTy _sH' ->
      undefined -- runServer (handle sH' (impl _))


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
    _ -> bugInGHC

handleResponse _ _ _ = bugInGHC

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
    ElField sing val :& rest ->
      headerEncodePair sing val : encodeHeaders' rest
