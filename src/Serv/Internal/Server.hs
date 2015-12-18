{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.Server where

import           Data.Function                ((&))
import           Data.Maybe
import           Data.Proxy
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.String
import           Data.Tagged
import           Data.Text                    (Text)
import           GHC.TypeLits
import qualified Network.HTTP.Types           as HTTP
import qualified Network.Wai                  as Wai
import qualified Serv.Header.Proxies          as Hp
import           Serv.Internal.Api
import           Serv.Internal.Api.Analysis
import qualified Serv.Internal.Cors           as Cors
import qualified Serv.Internal.Header         as Header
import           Serv.Internal.Pair
import           Serv.Internal.Rec
import           Serv.Internal.Server.Context (Context)
import qualified Serv.Internal.Server.Context as Context
import qualified Serv.Internal.Server.Error   as Error
import           Serv.Internal.Server.Type
import qualified Serv.Internal.URI            as URI
import qualified Serv.Internal.Verb           as Verb

-- Handling
-- ----------------------------------------------------------------------------

-- | The the core type function responsible for interpreting an 'Api' into a
-- functioning 'Server'. It defines a function 'handle' defined over all forms
-- of 'Api' types which consumes a parallel type defined by the associated
-- type family 'Impl'. If @api :: Api@ then @Impl api m@ is an "implementation"
-- of the 'Api''s server logic executing in the @m@ monad. Then, applying
-- 'handle' to a value of @'Impl' api@ results in a 'Server' which can be
-- executed as a 'Wai.Application'.

class Handling (spec :: k) where
  type Impl spec (m :: * -> *)
  handle :: Monad m => Proxy spec -> Impl spec m -> Server m

{-

TODO
----

- [X] 'Method verb headers body
- [T] 'CaptureHeaders headers api
- [T] 'CaptureQuery query api
- [T] 'CaptureBody ctypes value api
- [X] 'Raw

- [X] '[]
- [X] x ': xs

- [X] OneOf apis
- [X] Endpoint apis

- [X] Const s :> api
- [X] HeaderAs name value :> api
- [X] Seg name value :> api
- [X] Header name value :> api
- [X] Wildcard :> api
- [ ] Flag name :> api
- [ ] QueryParam name value :> api
- [X] Cors :> api

-}

encodeBody :: WaiResponse hdrs body => Context -> Response hdrs body -> ServerValue
encodeBody ctx resp =
  case acceptHdr of
    Left _ -> WaiResponse (waiResponse [] resp)
    Right acceptList ->
      WaiResponse (waiResponse acceptList resp)
  where
    (_, acceptHdr) = Context.examineHeader Hp.accept ctx

-- | 'GET' is special-cased to handle @HEAD@ semantics which cannot be
-- specified otherwise.
instance
  {-# OVERLAPPING  #-}
  WaiResponse headers body =>
  Handling ('Method 'Verb.GET headers body) where

  type Impl ('Method 'Verb.GET headers body) m =
    m (Response headers body)

  handle _ mresp = Server $ \ctx -> do
    let method = Context.method ctx
    case method of
      "GET" -> do
        resp <- mresp
        return (encodeBody ctx resp)
      "HEAD" -> do
        resp <- mresp
        let newResp = deleteBody resp
        return (WaiResponse (waiResponse [] newResp))
      _ -> routingError Error.NotFound

instance
  (Verb.ReflectVerb verb, WaiResponse headers body) =>
  Handling ('Method verb headers body) where

  type Impl ('Method verb headers body) m =
    m (Response headers body)

  handle _ mresp = Server $ \ctx -> do
    let method = Context.method ctx
        expected = Verb.reflectVerb (Proxy :: Proxy verb)
    if method /= Verb.standardName expected
       then routingError Error.NotFound
       else do
         resp <- mresp
         case snd (Context.examineHeader Hp.accept ctx) of
           Left _err ->
             routingError
             (Error.BadRequest
              (Just "could not parse acceptable content types"))
           Right accepts ->
             return (WaiResponse $ waiResponse accepts resp)

instance Handling method =>
  Handling ('CaptureHeaders (headers :: [Pair Header.HeaderName *]) method) where
  type Impl ('CaptureHeaders headers method) m =
    Rec headers -> Impl method m
  handle = undefined -- TODO

instance Handling method =>
  Handling ('CaptureQuery (query :: [Pair Symbol *]) method) where
  type Impl ('CaptureQuery query method) m =
    Rec query -> Impl method m
  handle = undefined -- TODO

instance Handling method =>
  Handling ('CaptureBody ctypes (value :: *) method) where
  type Impl ('CaptureBody ctypes value method) m =
    value -> Impl method m
  handle = undefined -- TODO

instance Handling 'Raw where
  type Impl 'Raw m = m Wai.Application
  handle _ impl = Server $ \_ -> do
    app <- impl
    return (Application app)

instance
  (VerbsOf methods,
   HeadersReturnedBy methods,
   HeadersExpectedOf methods,
   Handling methods)
  => Handling ('Endpoint ann (methods :: [Method *]))
  where
    type Impl ('Endpoint ann methods) m = Impl methods m
    handle Proxy impl = Server $ \ctx -> do
      let pathIsEmpty = Context.pathIsEmpty ctx
      if not pathIsEmpty
        then routingError Error.NotFound
        else do
          let method = Context.method ctx
              methodsProxy = Proxy :: Proxy methods
          if | method == HTTP.methodOptions ->
                 return $ defaultOptionsResponse verbs
                        & addHeaders (
                            fromMaybe [] $ Context.corsHeaders methodsProxy True ctx
                          )

             | verbMatch verbs method -> do
                 value <- runServer (handle (Proxy :: Proxy methods) impl) ctx
                 return $ value
                        & addHeaders (
                            fromMaybe [] $ Context.corsHeaders methodsProxy False ctx
                          )

             | otherwise ->
               -- TODO: Probably a double-check; trying the method implementations
               -- ought to fail this way, too
               routingError (Error.MethodNotAllowed (Set.toList verbs))
      where
        verbs = verbsOf (Proxy :: Proxy methods)
        addHeaders hdrs v =
          case v of
            WaiResponse resp ->
              WaiResponse (Wai.mapResponseHeaders (hdrs ++) resp)
            other -> other


-- | Is the request method in the set of verbs?
verbMatch :: Set Verb.Verb -> HTTP.Method -> Bool
verbMatch verbs methodname =
    case methodname of
      -- Special-casing the GET/HEAD overlap
      "HEAD" -> verbMatch verbs "GET"
      _ -> methodname `Set.member` Set.map Verb.standardName verbs

defaultOptionsResponse :: Set Verb.Verb -> ServerValue
defaultOptionsResponse verbs =
  -- TODO: Add CORS information
  WaiResponse
  $ Wai.responseLBS
    HTTP.ok200
    (catMaybes [Header.headerPair Hp.allow orderedVerbs])
    ""
  where
    allVerbs =
      if Set.member Verb.GET verbs
        then verbs & Set.insert Verb.HEAD
                   & Set.insert Verb.OPTIONS
        else verbs & Set.insert Verb.OPTIONS
    orderedVerbs = Set.toList allVerbs

instance Handling '[] where
  type Impl '[] m = m NotHere
  handle _ m = Server $ \_ -> do
    NotHere <- m
    routingError Error.NotFound

instance (Handling x, Handling xs) => Handling (x ': xs) where
  type Impl (x ': xs) m = Impl x m :<|> Impl xs m
  handle _ (l :<|> r) = lServer `orElse` rServer where
    lServer = handle (Proxy :: Proxy x) l
    rServer = handle (Proxy :: Proxy xs) r

instance Handling apis => Handling ('OneOf apis) where
  type Impl ('OneOf apis) m = Impl apis m
  handle _ = handle (Proxy :: Proxy apis)

instance (KnownSymbol s, Handling api) => Handling ('Const s ':> api) where
  type Impl ('Const s ':> api) m = Impl api m
  handle _ impl = Server $ \ctx -> do
    let (ctx', m) = Context.takeSegment ctx
        matchName = fromString (symbolVal (Proxy :: Proxy s))
        next = handle (Proxy :: Proxy api) impl
    case m of
      Nothing -> routingError Error.NotFound
      Just seg
        | seg /= matchName -> routingError Error.NotFound
        | otherwise -> runServer next ctx'

instance Handling api => Handling ('Wildcard ':> api) where
  type Impl ('Wildcard ':> api) m = [Text] -> Impl api m
  handle _ f = Server $ \ctx -> do
    let (ctx', segs) = Context.takeAllSegments ctx
    runServer (handle (Proxy :: Proxy api) (f segs)) ctx'

instance (Header.ReflectName n, KnownSymbol v, Handling api) => Handling ('HeaderAs n v ':> api) where
  type Impl ('HeaderAs s v ':> api) m = Impl api m
  handle _ impl = Server $ \ctx -> do
    let headerProxy = Proxy :: Proxy n
        headerValue = fromString (symbolVal (Proxy :: Proxy v))
        next = handle (Proxy :: Proxy api) impl
        (ctx', ok) = Context.expectHeader headerProxy headerValue ctx
    if ok
      then runServer next ctx'
      else routingError Error.NotFound

instance
  (Header.HeaderDecode n v, Handling api) => Handling ('Header n v ':> api)
  where
    type Impl ('Header n v ':> api) m = v -> Impl api m
    handle _ impl = Server $ \ctx -> do
      let headerProxy = Proxy :: Proxy n
          (ctx', m) = Context.examineHeader headerProxy ctx
          next = handle (Proxy :: Proxy api) . impl
      case m of
        Left parseError -> routingError (Error.BadRequest (Just parseError))
        Right value -> runServer (next value) ctx'

instance (URI.URIDecode v, Handling api) => Handling ('Seg n v ':> api) where
  type Impl ('Seg n v ':> api) m = Tagged n v -> Impl api m
  handle _ impl = Server $ \ctx -> do
    let (ctx', m) = Context.takeSegment ctx
        next = handle (Proxy :: Proxy api) . impl
    case m of
      Nothing -> routingError Error.NotFound
      Just seg -> case URI.uriDecode seg of
        Left _err -> routingError (Error.BadRequest Nothing)
        Right val -> runServer (next $ Tagged val) ctx'

instance (Handling api, Cors.CorsPolicy p) => Handling ('Cors p ':> api) where
  type Impl ('Cors p ':> api) m = Impl api m
  handle _ impl = Server $ \ctx ->
    let newCtx = ctx { Context.corsPolicies =
                         Cors.corsPolicy (Proxy :: Proxy p)
                         : Context.corsPolicies ctx }
    in runServer (handle (Proxy :: Proxy api) impl) newCtx

-- instance (ContentMatching cts ty, Handling api) => Handling ('CaptureBody cts ty ':> api) where

--   type Impl ('CaptureBody cts ty ':> api) m =
--     ty -> Impl api m

--   handle Proxy impl = Server $ do
--     (newContext, maybeHeader) <- asks $ Context.pullHeaderRaw HTTP.hContentType
--     let header = fromMaybe "application/octet-stream" maybeHeader
--     reqBody <- asks Context.body
--     case Media.mapContentMedia matches header of
--       Nothing -> throwError Error.UnsupportedMediaType
--       Just decoder -> case decoder reqBody of
--         Left err -> throwError $ Error.BadRequest (Just err)
--         Right val -> local (const newContext) (continue val)

--     where
--       matches = contentMatch (Proxy :: Proxy cts)
--       continue = runServer . handle (Proxy :: Proxy api) . impl
