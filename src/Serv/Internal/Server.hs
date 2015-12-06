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

import qualified Data.Set as Set
import           Data.Set (Set)
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString              as S
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           GHC.TypeLits
import           Network.HTTP.Media           (MediaType)
import qualified Network.HTTP.Media           as Media
import qualified Network.HTTP.Types as HTTP
import           Serv.Internal.Api
import           Serv.Internal.Interpretation
import           Serv.Internal.Response
import qualified Serv.Internal.Header as Header
import           Serv.Internal.Server.Context (Context)
import qualified Serv.Internal.Server.Context as Context
import qualified Serv.Internal.Server.Error   as Error
import           Serv.Internal.Server.Type
import qualified Network.Wai as Wai
import Data.Text (Text)


-- Handling
-- ----------------------------------------------------------------------------

-- | The the core type function responsible for interpreting an 'Api' into a
-- functioning 'Server'. It defines a function 'handle' defined over all forms
-- of 'Api' types which consumes a parallel type defined by the associated
-- type family 'Impl'. If @api :: Api@ then @Impl api m@ is an "implementation"
-- of the 'Api''s server logic executing in the @m@ monad. Then, applying
-- 'handle' to a value of @'Impl' api@ results in a 'Server' which can be
-- executed as a 'Wai.Application'.

class Handling (api :: k) where
  type Impl api (m :: * -> *)
  handle :: Monad m => Proxy api -> Impl api m -> Server m




-- Handling Endpoints
-- ----------------------------------------------------------------------------

-- Auto handle OPTIONS and CORS?
--
-- > The Allow entity-header field lists the set of methods supported
--   by the resource identified by the Request-URI. The purpose of this
--   field is strictly to inform the recipient of valid methods
--   associated with the resource. An Allow header field MUST be
--   present in a 405 (Method Not Allowed) response.


instance Handling ('[] :: [Method *]) where
  type Impl ('[] :: [Method *]) m = m NotHere
  handle = undefined

instance Handling ('Method verb headers body ': rs :: [Method *]) where
  type Impl ('Method verb headers body ': rs) m =
    m (Response headers body) :<|> Impl rs m
  handle = undefined


instance
  (HeadersOf methods, VerbsOf methods, Handling methods) =>
    Handling ('Endpoint methods)
  where

    type Impl ('Endpoint methods) m =
      Impl methods m

    handle Proxy impl = Server $ do
      pathIsEmpty <- asks Context.pathIsEmpty
      when (not pathIsEmpty) (throwError Error.NotFound)
      method <- asks Context.method
      requestHeaders <- asks Context.requestHeadersSeen
      case () of
        ()

          | method == HTTP.methodOptions ->
            return (defaultOptionsResponse verbs headers requestHeaders)

          | verbMatch verbs method ->
            runServer (handle (Proxy :: Proxy methods) impl)

          | otherwise ->
            throwError $ Error.MethodNotAllowed (Set.toList verbs)

      where
        verbs = verbsOf (Proxy :: Proxy methods)
        headers = headersOf (Proxy :: Proxy methods)

verbMatch :: Set Verb -> S.ByteString -> Bool
verbMatch verbs methodname =
  methodname `Set.member` Set.map (fromString . show) verbs

defaultOptionsResponse
  :: Set Verb -> Set HTTP.HeaderName -> Set HTTP.HeaderName -> Wai.Response
defaultOptionsResponse verbs _headers _requestHeaders =
  Wai.responseLBS
  HTTP.ok200
  [("Allow", headerEncodeBS (Proxy :: Proxy 'Header.Allow) (Set.toList verbs))]
  ""

-- instance Handling ('Endpoint '[]) where

--   type Impl ('Endpoint '[]) m =
--     m NotHere

--   handle Proxy m = Server $ do
--     NotHere <- lift (lift m)
--     throwError Error.NotFound

-- instance
--   Handling ('Endpoint methods) =>
--     Handling ('Endpoint ('Method verb headers body ': methods))

--   where

--     type Impl ('Endpoint ('Method verb headers body ': methods)) m =
--       m (Response headers body) :<|> Impl ('Endpoint methods) m

--     -- Very similar to the implementation of @Handling ('Choice apis)@
--     handle Proxy (m :<|> ms) =
--       goHere
--       `orElse`
--       handle (Proxy :: Proxy ('Endpoint methods)) ms

--       where

--         goHere = undefined m



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



-- Handling Choice
-- ----------------------------------------------------------------------------

instance Handling ('OneOf '[]) where
  type Impl ('OneOf '[]) m = NotHere
  handle Proxy NotHere = Server (throwError Error.NotFound)

instance
  (Handling api, Handling ('OneOf apis)) =>
    Handling ('OneOf (api ': apis))
  where
    type Impl ('OneOf (api ': apis)) m =
      Impl api m :<|> Impl ('OneOf apis) m

    handle Proxy (impl :<|> impls) =
      handle (Proxy :: Proxy api) impl
      `orElse`
      handle (Proxy :: Proxy ('OneOf apis)) impls



-- Handling Qualifiers
-- ----------------------------------------------------------------------------

instance
  (Header.ReflectName name, Handling api, URIDecode val) =>
    Handling ('CaptureHeader name val ':> api)

  where

    type Impl ('CaptureHeader name val ':> api) m =
      Maybe val -> Impl api m

    handle Proxy impl = Server $ do
      (newContext, mayHdr) <- asks $ Context.examineHeader headerName
      case mayHdr of
        Nothing -> local (const newContext) (continue Nothing)
        Just (Left parseError) -> throwError (Error.BadRequest (Just parseError))
        Just (Right val) -> local (const newContext) (continue $ Just val)

      where
        continue = runServer . handle (Proxy :: Proxy api) . impl
        headerName = (Header.reflectName (Proxy :: Proxy name))

instance
  (Header.ReflectName name, KnownSymbol val, Handling api) =>
    Handling ('MatchHeader name val ':> api)

  where

    type Impl ('MatchHeader name val ':> api) m =
      Impl api m

    handle Proxy impl = Server $ do
      (newContext, ok) <- asks
                          $ Context.expectHeader
                            headerName
                            (fromString headerValue)
      if ok
        then local (const newContext) continue
        else throwError
             (Error.BadRequest
              (Just $ "Header " ++ show headerName ++ " expected to take value " ++ headerValue))

      where
        Server continue = handle (Proxy :: Proxy api) impl
        headerName = Header.reflectName (Proxy :: Proxy name)
        headerValue = symbolVal (Proxy :: Proxy val)


instance (Handling api, KnownSymbol s) => Handling ('Seg s ':> api) where

  type Impl ('Seg s ':> api) m =
    Impl api m

  handle Proxy impl = Server $ do
    (newContext, maySeg) <- asks Context.takeSegment
    case maySeg of
      Nothing -> throwError Error.NotFound
      Just seg
        | seg == segToMatch -> local (const newContext) continue
        | otherwise -> throwError Error.NotFound

    where
      segToMatch = fromString (symbolVal (Proxy :: Proxy s))
      Server continue = handle (Proxy :: Proxy api) impl


instance (URIDecode ty, Handling api) => Handling ('CaptureSeg name ty ':> api) where

  type Impl ('CaptureSeg name ty ':> api) m =
    ty -> Impl api m

  handle Proxy impl = Server $ do
    (newContext, maySeg) <- asks Context.takeSegment
    case maySeg of
      Nothing -> throwError Error.NotFound
      Just seg -> case uriDecode seg of
        Left _err -> throwError (Error.BadRequest Nothing)
        Right val -> local (const newContext) (continue val)

    where
      continue = runServer . handle (Proxy :: Proxy api) . impl


instance Handling api => Handling ('CaptureContext ':> api) where

  type Impl ('CaptureContext ':> api) m = Context -> Impl api m

  handle Proxy impl = Server $ do
    ctx <- ask
    runServer (handle (Proxy :: Proxy api) (impl ctx))


instance Handling api => Handling ('CaptureWildcard ':> api) where

  type Impl ('CaptureWildcard ':> api) m = [Text] -> Impl api m

  handle Proxy impl = Server $ do
    ctx <- ask
    let (newContext, path) = Context.takeAllSegments ctx
    local (const newContext) (continue path)

    where continue = runServer . handle (Proxy :: Proxy api) . impl



-- Type-level Computations
-- ----------------------------------------------------------------------------



class ContentMatching cts ty where
  contentMatch :: Proxy cts -> [(MediaType, S.ByteString -> Either String ty)]

instance ContentMatching '[] ty where
  contentMatch Proxy = []

instance

  (MimeDecode ct ty, ContentMatching cts ty) =>
    ContentMatching (ct ': cts) ty

  where

    contentMatch Proxy =
      (mtype, decode) : contentMatch (Proxy :: Proxy cts)
      where
        mtype = mediaType (Proxy :: Proxy ct)
        decode = mimeDecode (Proxy :: Proxy ct)



class VerbsOf methods where
  verbsOf :: Proxy methods -> Set Verb

instance VerbsOf '[] where
  verbsOf Proxy = Set.empty

instance
  (ReflectVerb verb, VerbsOf methods) =>
    VerbsOf ('Method verb headers body ': methods)
  where
    verbsOf Proxy =
      Set.insert
      (reflectVerb (Proxy :: Proxy verb))
      (verbsOf (Proxy :: Proxy methods))



class HeadersOf methods where
  headersOf :: Proxy methods -> Set HTTP.HeaderName

instance HeadersOf '[] where
  headersOf Proxy = Set.empty

instance
  (HeadersOf methods) =>
    HeadersOf ('Method verb '[] body ': methods)
  where
    headersOf Proxy = headersOf (Proxy :: Proxy methods)

instance
  (HeadersOf ('Method verb headers body ': methods), Header.ReflectName name) =>
    HeadersOf ('Method verb ( '(name, ty) ': headers) body ': methods)
  where
    headersOf Proxy =
      Set.insert
      (Header.reflectName (Proxy :: Proxy name))
      (headersOf (Proxy :: Proxy ('Method verb headers body ': methods)))
