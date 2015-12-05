{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
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

module Serv.Internal.Server.Context where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           Data.Tagged
import           Data.Text (Text)
import           GHC.TypeLits
import           Network.HTTP.Media (MediaType)
import           Network.HTTP.Types (HeaderName, Status)
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Media as Media
import qualified Network.HTTP.Types.Header as Header
import qualified Network.Wai as Wai

import           Serv.Internal.Server.Config
import           Serv.Internal.Interpretation

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
        Just (Right (RawText observation)) -> observation == value

    mayVal = lookup name headers
    newContext = ctx { headersExpected = (name, Just value) : headersExpected ctx }
    headers = Wai.requestHeaders req
    req = request ctx




-- Util
-- ----------------------------------------------------------------------------

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a
