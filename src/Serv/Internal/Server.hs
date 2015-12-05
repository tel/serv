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
import           Control.Monad.Trans.Either
import qualified Data.ByteString              as S
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           GHC.TypeLits
import           Network.HTTP.Media           (MediaType)
import qualified Network.HTTP.Media           as Media
import qualified Network.HTTP.Types.Header    as Header
import qualified Network.Wai                  as Wai
import           Serv.Internal.Api
import           Serv.Internal.Interpretation
import           Serv.Internal.Response
import           Serv.Internal.Server.Context (Context)
import qualified Serv.Internal.Server.Context as Context
import           Serv.Internal.Server.Error   (RoutingError)
import qualified Serv.Internal.Server.Error   as Error

data FailServer = FailServer
data a :<|> b = a :<|> b

newtype Server m =
  Server
  { runServer :: ReaderT Context (EitherT RoutingError m) Wai.Response }

-- | Run a server to its roots
rootServer :: Context -> Server m -> m (Either RoutingError Wai.Response)
rootServer ctx = runEitherT . flip runReaderT ctx . runServer

orElse :: Monad m => Server m -> Server m -> Server m
orElse a b = Server $ do
  ctx <- ask
  va <- lift (lift (rootServer ctx a))
  case va of
    Left Error.NotFound -> runServer b
    Left _ -> runServer a -- all other errors can be ignored (?)
    Right _ -> runServer a


-- Handling
-- ----------------------------------------------------------------------------

class Handling (api :: Api *) where
  type Impl api (m :: * -> *)
  handle :: Monad m => Proxy api -> Impl api m -> Server m




-- Handling APIs
-- ----------------------------------------------------------------------------

-- Auto handle OPTIONS and CORS?
--
-- > The Allow entity-header field lists the set of methods supported
--   by the resource identified by the Request-URI. The purpose of this
--   field is strictly to inform the recipient of valid methods
--   associated with the resource. An Allow header field MUST be
--   present in a 405 (Method Not Allowed) response.

instance Handling ('Endpoint '[]) where

  type Impl ('Endpoint '[]) m =
    m FailServer

  handle Proxy m = Server $ do
    FailServer <- lift (lift m)
    throwError Error.NotFound

instance
  Handling ('Endpoint methods) =>
    Handling ('Endpoint ('Method verb headers body ': methods))

  where

    type Impl ('Endpoint ('Method verb headers body ': methods)) m =
      m (Response headers body) :<|> Impl ('Endpoint methods) m

    -- Very similar to the implementation of @Handling ('Choice apis)@
    handle Proxy (m :<|> ms) =
      goHere
      `orElse`
      handle (Proxy :: Proxy ('Endpoint methods)) ms

      where

        goHere = undefined m

instance Handling ('OneOf '[]) where

  type Impl ('OneOf '[]) m =
    FailServer

  handle Proxy FailServer = Server (throwError Error.NotFound)

instance (Handling api, Handling ('OneOf apis)) => Handling ('OneOf (api ': apis)) where

  type Impl ('OneOf (api ': apis)) m =
     Impl api m :<|> Impl ('OneOf apis) m

  handle Proxy (impl :<|> impls) =
    handle (Proxy :: Proxy api) impl
    `orElse`
    handle (Proxy :: Proxy ('OneOf apis)) impls






-- Handling Qualifiers
-- ----------------------------------------------------------------------------

instance
  (KnownSymbol name, Handling api, URIDecode val) =>
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
        headerName = fromString (symbolVal (Proxy :: Proxy name))

instance
  (KnownSymbol name, KnownSymbol val, Handling api) =>
    Handling ('MatchHeader name val ':> api)

  where

    type Impl ('MatchHeader name val ':> api) m =
      Impl api m

    handle Proxy impl = Server $ do
      (newContext, ok) <- asks $ Context.expectHeader (fromString headerName) (fromString headerValue)
      if ok
        then local (const newContext) continue
        else throwError (Error.BadRequest (Just $ "Header " ++ headerName ++ " expected to take value " ++ headerValue))

      where
        Server continue = handle (Proxy :: Proxy api) impl
        headerName = symbolVal (Proxy :: Proxy name)
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


instance (ContentMatching cts ty, Handling api) => Handling ('CaptureBody cts ty ':> api) where

  type Impl ('CaptureBody cts ty ':> api) m =
    ty -> Impl api m

  handle Proxy impl = Server $ do
    (newContext, maybeHeader) <- asks $ Context.pullHeaderRaw Header.hContentType
    let header = fromMaybe "application/octet-stream" maybeHeader
    reqBody <- asks Context.body
    case Media.mapContentMedia matches header of
      Nothing -> throwError Error.UnsupportedMediaType
      Just decoder -> case decoder reqBody of
        Left err -> throwError $ Error.BadRequest (Just err)
        Right val -> local (const newContext) (continue val)

    where
      matches = contentMatch (Proxy :: Proxy cts)
      continue = runServer . handle (Proxy :: Proxy api) . impl



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
