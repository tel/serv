{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           Data.Text (Text)
import           GHC.TypeLits
import           Network.HTTP.Media (MediaType)
import           Network.HTTP.Types (HeaderName)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Media as Media
import qualified Network.HTTP.Types.Header as Header
import qualified Network.Wai as Wai

data ContentType where
  ContentType :: m -> Nat -> ContentType

data Verb
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE

data ResponseHeader = ResponseHeader Symbol

data Response ty where
  RespondZero :: Verb -> [ResponseHeader] -> Response ty
  RespondOne :: Verb -> [ContentType] -> [ResponseHeader] -> ty -> Response ty

data API ty where
  Seg :: Symbol -> API ty
  MatchHeader :: Symbol -> Symbol -> API ty

  CaptureSeg :: Symbol -> ty -> API ty
  CaptureHeader :: Symbol -> ty -> API ty
  CaptureBody :: [ContentType] -> ty -> API ty
  CaptureContext :: API ty

  -- These two are elided for the moment as we're not using query strings yet;
  -- they're not terrifically hard to implement, though.
  --
  --
  -- CaptureFlag :: Symbol -> API ty
  -- CaptureParam :: Symbol -> ty -> API ty

  (:>) :: API ty -> API ty -> API ty
  OneOf :: [API ty] -> API ty

  Endpoint :: [Response ty] -> API ty

  --
  --   Raw :: API ty
  --
  --
  --
  -- To achieve this (or an UPGRADE endpoint) we need to augment Server m to
  -- internalize Wai.Application values. Something like
  --
  --   Server m ~ Context -> Either3T RoutingErr Wai.Application m Wai.Result
  --
  -- indicating that instead of returning a result sometimes we "upgrade" to a
  -- whole new Wai.Application which will from here on out handle the request.
  --
  -- The previous design ought to be easy to transform into a Wai.Application
  --
  --   Server IO -> Wai.Application
  --
  -- since it can just "hook in" the new application when needed.




-- Type interpretation

class URIEncode a where
  uriEncode :: a -> Text

class URIDecode a where
  uriDecode :: Text -> Either String a

instance URIDecode Text where
  uriDecode text = Right text

fromByteString :: URIDecode a => S8.ByteString -> Either String a
fromByteString s = case Text.decodeUtf8' s of
  Left _err -> Left "could not parse UTF8 string"
  Right a -> uriDecode a


class HasMediaType ty where
  mediaType :: Proxy ty -> MediaType

class HasMediaType ty => MimeEncode ty val where
  mimeEncode :: Proxy ty -> val -> S.ByteString

class HasMediaType ty => MimeDecode ty val where
  mimeDecode :: Proxy ty -> S.ByteString -> Either String val

-- Server

data FailServer = FailServer
data a :& b = a :& b

data RoutingErr
  = ENotFound
  | EBadRequest (Maybe String)
  | EUnsupportedMediaType

-- | An ignorable error is one which backtracks the routing search
-- instead of forcing a response.
ignorableErr :: RoutingErr -> Bool
ignorableErr ENotFound = True
ignorableErr _ = False

data Config =
  Config

data Context =
  Context
  { request :: Wai.Request
  , pathZipper :: ([Text], [Text])
  , headersExpected :: [(HeaderName, Maybe Text)]
  , config :: Config

    -- cached via strictRequestBody so that we don't have to deal with multiple
    -- request body pulls affecting one another; this defeats partial and lazy body
    -- loading, BUT the style of API description we're talking about here isn't really
    -- amenable to that sort of thing anyway.
    --
    -- also note that we really need to compute this using Lazy IO; otherwise,
    -- we'll have to be handling the partial request/respond dance from the get-go.

  , body :: S.ByteString
  }

-- | Pop a segment off the URI and produce a new context for "beyond" that segment
takeSegment :: Context -> (Context, Maybe Text)
takeSegment ctx =
  (stepContext ctx, safeHead fore)
  where
    (_, fore) = pathZipper ctx

-- | Move the context down the URI segment listing one step if possible.
stepContext :: Context -> Context
stepContext ctx =
  case fore of
    [] -> ctx
    seg : rest -> ctx { pathZipper = (seg : hind, rest) }

  where
    (hind, fore) = pathZipper ctx

-- | Pull a Header raw from the context, updating it to note that we looked
pullHeaderRaw :: HeaderName -> Context -> (Context, Maybe S.ByteString)
pullHeaderRaw name ctx =
  (newContext, lookup name headers)
  where
    newContext = ctx { headersExpected = (name, Nothing) : headersExpected ctx }
    headers = Wai.requestHeaders req
    req = request ctx

-- | Pull a header value from the context, updating it to note that we looked
examineHeader :: URIDecode a => HeaderName -> Context -> (Context, Maybe (Either String a))
examineHeader name ctx =
  (newContext, fromByteString <$> rawString )
  where (newContext, rawString) = pullHeaderRaw name ctx

-- | Match a header value in the context, updating it to show that we looked
expectHeader :: HeaderName -> Text -> Context -> (Context, Bool)
expectHeader name value ctx =
  (newContext, valOk)

  where
    valOk =
      case fmap fromByteString mayVal of
        Nothing -> False
        Just (Left _) -> False
        Just (Right observation) -> observation == value

    mayVal = lookup name headers
    newContext = ctx { headersExpected = (name, Just value) : headersExpected ctx }
    headers = Wai.requestHeaders req
    req = request ctx

newtype Server m =
  Server
  { runServer :: ReaderT Context (EitherT RoutingErr m) Wai.Response }

-- | Run a server to its roots
rootServer :: Context -> Server m -> m (Either RoutingErr Wai.Response)
rootServer ctx = runEitherT . flip runReaderT ctx . runServer

orElse :: Monad m => Server m -> Server m -> Server m
orElse a b = Server $ do
  ctx <- ask
  va <- lift (lift (rootServer ctx a))
  case va of
    Left ENotFound -> runServer b
    Left _ -> runServer a -- all other errors can be ignored (?)
    Right _ -> runServer a




-- Handling

class Handling (api :: API *) where
  type Impl api (m :: * -> *)
  handle :: Monad m => Proxy api -> Impl api m -> Server m




instance Handling ('OneOf '[]) where

  type Impl ('OneOf '[]) m =
    FailServer

  handle Proxy FailServer = Server (throwError ENotFound)

instance (Handling api, Handling ('OneOf apis)) => Handling ('OneOf (api ': apis)) where

  type Impl ('OneOf (api ': apis)) m =
     Impl api m :& Impl ('OneOf apis) m

  handle Proxy (impl :& impls) =
    handle (Proxy :: Proxy api) impl
    `orElse`
    handle (Proxy :: Proxy ('OneOf apis)) impls




instance (KnownSymbol name, Handling api, URIDecode val) => Handling ('CaptureHeader name val ':> api) where

  type Impl ('CaptureHeader name val ':> api) m =
    Maybe val -> Impl api m

  handle Proxy impl = Server $ do
    (newContext, mayHdr) <- asks $ examineHeader headerName
    case mayHdr of
      Nothing -> local (const newContext) (continue Nothing)
      Just (Left parseError) -> throwError (EBadRequest (Just parseError))
      Just (Right val) -> local (const newContext) (continue $ Just val)

    where
      continue = runServer . handle (Proxy :: Proxy api) . impl
      headerName = fromString (symbolVal (Proxy :: Proxy name))

instance (KnownSymbol name, KnownSymbol val, Handling api) => Handling ('MatchHeader name val ':> api) where

  type Impl ('MatchHeader name val ':> api) m =
    Impl api m

  handle Proxy impl = Server $ do
    (newContext, ok) <- asks $ expectHeader (fromString headerName) (fromString headerValue)
    if ok
      then local (const newContext) continue
      else throwError (EBadRequest (Just $ "Header " ++ headerName ++ " expected to take value " ++ headerValue))

    where
      Server continue = handle (Proxy :: Proxy api) impl
      headerName = symbolVal (Proxy :: Proxy name)
      headerValue = symbolVal (Proxy :: Proxy val)

instance (Handling api, KnownSymbol s) => Handling ('Seg s ':> api) where

  type Impl ('Seg s ':> api) m =
    Impl api m

  handle Proxy impl = Server $ do
    (newContext, maySeg) <- asks takeSegment
    case maySeg of
      Nothing -> throwError ENotFound
      Just seg
        | seg == segToMatch -> local (const newContext) continue
        | otherwise -> throwError ENotFound

    where
      segToMatch = fromString (symbolVal (Proxy :: Proxy s))
      Server continue = handle (Proxy :: Proxy api) impl

instance (URIDecode ty, Handling api) => Handling ('CaptureSeg name ty ':> api) where

  type Impl ('CaptureSeg name ty ':> api) m =
    ty -> Impl api m

  handle Proxy impl = Server $ do
    (newContext, maySeg) <- asks takeSegment
    case maySeg of
      Nothing -> throwError ENotFound
      Just seg -> case uriDecode seg of
        Left _err -> throwError (EBadRequest Nothing)
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
    (newContext, maybeHeader) <- asks $ pullHeaderRaw Header.hContentType
    let header = fromMaybe "application/octet-stream" maybeHeader
    reqBody <- asks body
    case Media.mapContentMedia matches header of
      Nothing -> throwError EUnsupportedMediaType
      Just decoder -> case decoder reqBody of
        Left err -> throwError $ EBadRequest (Just err)
        Right val -> local (const newContext) (continue val)

    where
      matches = contentMatch (Proxy :: Proxy cts)
      continue = runServer . handle (Proxy :: Proxy api) . impl

  -- Endpoint :: [Response ty] -> API ty


-- Type-level Util

class ContentMatching cts ty where
  contentMatch :: Proxy cts -> [(MediaType, S.ByteString -> Either String ty)]

instance ContentMatching '[] ty where
  contentMatch Proxy = []

instance (MimeDecode ct ty, ContentMatching cts ty) => ContentMatching (ct ': cts) ty where
  contentMatch Proxy =
    (mtype, decode) : contentMatch (Proxy :: Proxy cts)
    where
      mtype = mediaType (Proxy :: Proxy ct)
      decode = mimeDecode (Proxy :: Proxy ct)


-- Types

data JSON

data User = User { userName :: String }

newtype LogOutTime = LogOutTime Int




-- Example

type TheAPI
  = 'OneOf
    '[ 'Seg "log-out" ':> 'Endpoint LogOutResponses
     , 'Seg "user"    ':> 'Endpoint UserResponses
     ]

type UserResponses
  = '[ 'RespondOne
       'GET
       '[ 'ContentType JSON 0 ]
       '[ 'ResponseHeader "ETag" ]
       User

     , 'RespondZero 'DELETE '[]
     ]


type LogOutResponses
  = '[ 'RespondOne
       'GET
       '[ 'ContentType JSON 0 ]
       '[]
       LogOutTime
     ]





-- Util


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a
