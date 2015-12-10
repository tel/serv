{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Serv.Internal.Server where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Set (Set)
import           Data.String
import           Data.Text (Text)
import           GHC.TypeLits
import           Network.HTTP.Media           (MediaType)
import           Serv.Internal.Api
import           Serv.Internal.Server.Context (Context)
import           Serv.Internal.Server.Type
import qualified Data.ByteString              as S
import qualified Data.Set as Set
import qualified Network.HTTP.Media           as Media
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Serv.Internal.Header as Header
import qualified Serv.Internal.Server.Context as Context
import qualified Serv.Internal.Server.Error   as Error
import qualified Serv.Internal.URI as URI
import qualified Serv.Internal.MediaType as MediaType
import qualified Serv.Internal.Verb as Verb


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

- [ ] 'Handling verb headers body
- [ ] 'CaptureHeaders headers api
- [ ] 'CaptureBody ctypes value api
- [ ] 'Raw

- [X] '[]
- [X] x ': xs

- [X] OneOf apis
- [ ] Endpoint apis

- [X] Seg s :> api
- [X] HeaderAs name value :> api
- [ ] Capture name value :> api
- [X] Header name value :> api
- [X] Wildcard :> api
- [ ] Flag name :> api
- [ ] QueryParam name value :> api

-}

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

instance (KnownSymbol s, Handling api) => Handling ('Seg s ':> api) where
  type Impl ('Seg s ':> api) m = Impl api m
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
    type Impl ('Header n v ':> api) m = Maybe v -> Impl api m
    handle _ impl = Server $ \ctx -> do
      let headerProxy = Proxy :: Proxy n
          (ctx', m) = Context.examineHeader headerProxy ctx
          next = handle (Proxy :: Proxy api) . impl
      case m of
        Nothing -> runServer (next Nothing) ctx'
        Just (Left parseError) -> routingError (Error.BadRequest (Just parseError))
        Just (Right value) -> runServer (next (Just value)) ctx'


-- Handling Endpoints
-- ----------------------------------------------------------------------------

-- Auto handle OPTIONS and CORS?
--
-- > The Allow entity-header field lists the set of methods supported
--   by the resource identified by the Request-URI. The purpose of this
--   field is strictly to inform the recipient of valid methods
--   associated with the resource. An Allow header field MUST be
--   present in a 405 (Method Not Allowed) response.

-- instance
--   Verb.ReflectVerb verb =>
--     Handling ('Method verb headers body :: Method *)
--   where
--     type Impl ('Method verb headers body) m =
--       m (Response headers body)

--     handle Proxy mResponse = Server $ do

--       method <- asks Context.method
--       when (method /= Verb.standardHeader verb)
--         (throwError $ Error.MethodNotAllowed [verb])

--       response <- lift (lift mResponse)
--       _ response

--       where
--         verb = Verb.reflectVerb (Proxy :: Proxy verb)

-- instance Handling ('[] :: [Method *]) where
--   type Impl ('[] :: [Method *]) m = m NotHere
--   handle = undefined

-- instance Handling ('Method verb headers body ': rs :: [Method *]) where
--   type Impl ('Method verb headers body ': rs) m =
--     m (Response headers body) :<|> Impl rs m
--   handle = undefined


-- instance
--   (HeadersOf methods, VerbsOf methods, Handling methods) =>
--     Handling ('Endpoint methods)
--   where

--     type Impl ('Endpoint methods) m =
--       Impl methods m

--     handle Proxy impl = Server $ do
--       pathIsEmpty <- asks Context.pathIsEmpty
--       when (not pathIsEmpty) (throwError Error.NotFound)
--       method <- asks Context.method
--       requestHeaders <- asks Context.requestHeadersSeen
--       case () of
--         ()

--           | method == HTTP.methodOptions ->
--             return (defaultOptionsResponse verbs headers requestHeaders)

--           | verbMatch verbs method ->
--             runServer (handle (Proxy :: Proxy methods) impl)

--           | otherwise ->
--             -- TODO: Probably a double-check; trying the method implementations
--             -- ought to fail this way, too
--             throwError $ Error.MethodNotAllowed (Set.toList verbs)

--       where
--         verbs = verbsOf (Proxy :: Proxy methods)
--         headers = headersOf (Proxy :: Proxy methods)

-- -- | Is the request method in the set of verbs?
-- verbMatch :: Set Verb.Verb -> HTTP.Method -> Bool
-- verbMatch verbs methodname =
--   methodname `Set.member` Set.map Verb.standardHeader verbs

-- defaultOptionsResponse
--   :: Set Verb.Verb -> Set HTTP.HeaderName -> Set HTTP.HeaderName -> Wai.Response
-- defaultOptionsResponse verbs _headers _requestHeaders =
--   Wai.responseLBS
--   HTTP.ok200
--   [("Allow", Header.headerEncodeBS (Proxy :: Proxy 'Header.Allow) (Set.toList verbs))]
--   ""

-- -- instance Handling ('Endpoint '[]) where

-- --   type Impl ('Endpoint '[]) m =
-- --     m NotHere

-- --   handle Proxy m = Server $ do
-- --     NotHere <- lift (lift m)
-- --     throwError Error.NotFound

-- -- instance
-- --   Handling ('Endpoint methods) =>
-- --     Handling ('Endpoint ('Method verb headers body ': methods))

-- --   where

-- --     type Impl ('Endpoint ('Method verb headers body ': methods)) m =
-- --       m (Response headers body) :<|> Impl ('Endpoint methods) m

-- --     -- Very similar to the implementation of @Handling ('Choice apis)@
-- --     handle Proxy (m :<|> ms) =
-- --       goHere
-- --       `orElse`
-- --       handle (Proxy :: Proxy ('Endpoint methods)) ms

-- --       where

-- --         goHere = undefined m



-- -- instance (ContentMatching cts ty, Handling api) => Handling ('CaptureBody cts ty ':> api) where

-- --   type Impl ('CaptureBody cts ty ':> api) m =
-- --     ty -> Impl api m

-- --   handle Proxy impl = Server $ do
-- --     (newContext, maybeHeader) <- asks $ Context.pullHeaderRaw HTTP.hContentType
-- --     let header = fromMaybe "application/octet-stream" maybeHeader
-- --     reqBody <- asks Context.body
-- --     case Media.mapContentMedia matches header of
-- --       Nothing -> throwError Error.UnsupportedMediaType
-- --       Just decoder -> case decoder reqBody of
-- --         Left err -> throwError $ Error.BadRequest (Just err)
-- --         Right val -> local (const newContext) (continue val)

-- --     where
-- --       matches = contentMatch (Proxy :: Proxy cts)
-- --       continue = runServer . handle (Proxy :: Proxy api) . impl



-- -- Handling Choice
-- -- ----------------------------------------------------------------------------

-- instance Handling ('OneOf '[]) where
--   type Impl ('OneOf '[]) m = NotHere
--   handle Proxy NotHere = Server (throwError Error.NotFound)

-- instance
--   (Handling api, Handling ('OneOf apis)) =>
--     Handling ('OneOf (api ': apis))
--   where
--     type Impl ('OneOf (api ': apis)) m =
--       Impl api m :<|> Impl ('OneOf apis) m

--     handle Proxy (impl :<|> impls) =
--       handle (Proxy :: Proxy api) impl
--       `orElse`
--       handle (Proxy :: Proxy ('OneOf apis)) impls



-- -- Handling Qualifiers
-- -- ----------------------------------------------------------------------------

-- instance
--   (Header.ReflectName name, Handling api, URI.URIDecode val) =>
--     Handling ('CaptureHeader name val ':> api)

--   where

--     type Impl ('CaptureHeader name val ':> api) m =
--       Maybe val -> Impl api m

--     handle Proxy impl = Server $ do
--       (newContext, mayHdr) <- asks $ Context.examineHeader headerName
--       case mayHdr of
--         Nothing -> local (const newContext) (continue Nothing)
--         Just (Left parseError) -> throwError (Error.BadRequest (Just parseError))
--         Just (Right val) -> local (const newContext) (continue $ Just val)

--       where
--         continue = runServer . handle (Proxy :: Proxy api) . impl
--         headerName = (Header.reflectName (Proxy :: Proxy name))

-- instance
--   (Header.ReflectName name, KnownSymbol val, Handling api) =>
--     Handling ('MatchHeader name val ':> api)

--   where

--     type Impl ('MatchHeader name val ':> api) m =
--       Impl api m

--     handle Proxy impl = Server $ do
--       (newContext, ok) <- asks
--                           $ Context.expectHeader
--                             headerName
--                             (fromString headerValue)
--       if ok
--         then local (const newContext) continue
--         else throwError
--              (Error.BadRequest
--               (Just $ "Header " ++ show headerName ++ " expected to take value " ++ headerValue))

--       where
--         Server continue = handle (Proxy :: Proxy api) impl
--         headerName = Header.reflectName (Proxy :: Proxy name)
--         headerValue = symbolVal (Proxy :: Proxy val)


-- instance (Handling api, KnownSymbol s) => Handling ('Seg s ':> api) where

--   type Impl ('Seg s ':> api) m =
--     Impl api m

--   handle Proxy impl = Server $ do
--     (newContext, maySeg) <- asks Context.takeSegment
--     case maySeg of
--       Nothing -> throwError Error.NotFound
--       Just seg
--         | seg == segToMatch -> local (const newContext) continue
--         | otherwise -> throwError Error.NotFound

--     where
--       segToMatch = fromString (symbolVal (Proxy :: Proxy s))
--       Server continue = handle (Proxy :: Proxy api) impl


-- instance (URI.URIDecode ty, Handling api) => Handling ('CaptureSeg name ty ':> api) where

--   type Impl ('CaptureSeg name ty ':> api) m =
--     ty -> Impl api m

--   handle Proxy impl = Server $ do
--     (newContext, maySeg) <- asks Context.takeSegment
--     case maySeg of
--       Nothing -> throwError Error.NotFound
--       Just seg -> case URI.uriDecode seg of
--         Left _err -> throwError (Error.BadRequest Nothing)
--         Right val -> local (const newContext) (continue val)

--     where
--       continue = runServer . handle (Proxy :: Proxy api) . impl


-- instance Handling api => Handling ('CaptureContext ':> api) where

--   type Impl ('CaptureContext ':> api) m = Context -> Impl api m

--   handle Proxy impl = Server $ do
--     ctx <- ask
--     runServer (handle (Proxy :: Proxy api) (impl ctx))


-- instance Handling api => Handling ('CaptureWildcard ':> api) where

--   type Impl ('CaptureWildcard ':> api) m = [Text] -> Impl api m

--   handle Proxy impl = Server $ do
--     ctx <- ask
--     let (newContext, path) = Context.takeAllSegments ctx
--     local (const newContext) (continue path)

--     where continue = runServer . handle (Proxy :: Proxy api) . impl



-- -- Type-level Computations
-- -- ----------------------------------------------------------------------------


-- class VerbsOf methods where
--   verbsOf :: Proxy methods -> Set Verb.Verb

-- instance VerbsOf '[] where
--   verbsOf Proxy = Set.empty

-- instance
--   (Verb.ReflectVerb verb, VerbsOf methods) =>
--     VerbsOf ('Method verb headers body ': methods)
--   where
--     verbsOf Proxy =
--       Set.insert
--       (Verb.reflectVerb (Proxy :: Proxy verb))
--       (verbsOf (Proxy :: Proxy methods))



-- class HeadersOf methods where
--   headersOf :: Proxy methods -> Set HTTP.HeaderName

-- instance HeadersOf '[] where
--   headersOf _ = Set.empty

-- instance (HeadersOf r, HeadersOf rs) => HeadersOf (r ': rs) where
--   headersOf _ = headersOf (Proxy :: Proxy r) <> headersOf (Proxy :: Proxy rs)

-- instance HeadersOf headers => HeadersOf ('Method verb headers body) where
--   headersOf Proxy = headersOf (Proxy :: Proxy headers)

-- instance HeadersOf method => HeadersOf ('WithRequestBody ctypes a method ': methods) where
--   headersOf Proxy = headersOf (Proxy :: Proxy method)

-- instance
--   (HeadersOf headers, HeadersOf method) =>
--     HeadersOf ('WithRequestHeaders headers method)
--   where
--     headersOf Proxy =
--       headersOf (Proxy :: Proxy headers)
--       <>
--       headersOf (Proxy :: Proxy method)

-- instance Header.ReflectName name => HeadersOf ( '(name, ty) :: (Header.Name, *) ) where
--   headersOf _ = Set.singleton (Header.reflectName (Proxy :: Proxy name))
