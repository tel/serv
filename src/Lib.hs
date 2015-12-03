{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Data.Proxy
import           Data.String
import           Data.Text (Text)
import           GHC.TypeLits
import           Network.HTTP.Types (HeaderName)
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
  CaptureFlag :: Symbol -> API ty
  CaptureParam :: Symbol -> ty -> API ty
  CaptureBody :: [ContentType] -> ty -> API ty
  CaptureContext :: API ty

  (:>) :: API ty -> API ty -> API ty
  Choice :: [API ty] -> API ty

  Endpoint :: [Response ty] -> API ty






-- Type interpretation


class FromText a where
  fromText :: Text -> Either String a

instance FromText Text where
  fromText text = Right text

fromByteString :: FromText a => S8.ByteString -> Either String a
fromByteString s = case Text.decodeUtf8' s of
  Left _err -> Left "could not parse UTF8 string"
  Right a -> fromText a





-- Server

data RoutingErr
  = ENotFound
  | EBadRequest (Maybe String)

data Config =
  Config

data Context =
  Context
  { request :: Wai.Request
  , pathZipper :: ([Text], [Text])
  , headersExpected :: [(HeaderName, Maybe Text)]
  , config :: Config
  }

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

-- | Pop a segment off the URI and produce a new context for "beyond" that segment
takeSegment :: Context -> (Context, Maybe Text)
takeSegment ctx =
  (stepContext ctx, safeHead fore)
  where
    (rear, fore) = pathZipper ctx

-- | Move the context down the URI segment listing one step if possible.
stepContext :: Context -> Context
stepContext ctx =
  case fore of
    [] -> ctx
    seg : rest -> ctx { pathZipper = (seg : hind, rest) }

  where
    (hind, fore) = pathZipper ctx

-- | Pull a header value from the context, updating it to note that we looked
examineHeader :: FromText a => HeaderName -> Context -> (Context, Maybe (Either String a))
examineHeader name ctx =
  (newContext, fromByteString <$> lookup name headers )
  where
    newContext = ctx { headersExpected = (name, Nothing) : headersExpected ctx }
    headers = Wai.requestHeaders req
    req = request ctx

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

class Handling (api :: API *) where
  type Impl api (m :: * -> *)
  handle :: Monad m => Proxy api -> Impl api m -> Server m

instance (KnownSymbol name, Handling api, FromText val) => Handling ('CaptureHeader name val ':> api) where
  type Impl ('CaptureHeader name val ':> api) m = Maybe val -> Impl api m
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
  type Impl ('MatchHeader name val ':> api) m = Impl api m
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
  type Impl ('Seg s ':> api) m = Impl api m
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

instance (FromText ty, Handling api) => Handling ('CaptureSeg name ty ':> api) where
  type Impl ('CaptureSeg name ty ':> api) m = ty -> Impl api m
  handle Proxy impl = Server $ do
    (newContext, maySeg) <- asks takeSegment
    case maySeg of
      Nothing -> throwError ENotFound
      Just seg -> case fromText seg of
        Left err -> throwError (EBadRequest Nothing)
        Right val -> local (const newContext) (continue val)

    where
      continue = runServer . handle (Proxy :: Proxy api) . impl

  -- Choice :: [API ty] -> API ty

  -- CaptureBody :: [ContentType] -> ty -> API ty

  -- CaptureFlag :: Symbol -> API ty
  -- CaptureParam :: Symbol -> ty -> API ty
  -- CaptureContext :: API ty


  -- Endpoint :: [Response ty] -> API ty


-- Types

data JSON

data User = User { name :: String }

newtype LogOutTime = LogOutTime Int




-- Example

type TheAPI
  = 'Choice
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
