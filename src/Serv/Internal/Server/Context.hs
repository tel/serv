{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Serv.Internal.Server.Context where

import qualified Data.ByteString              as S
import           Data.Text                    (Text)
import qualified Network.Wai                  as Wai

import           Network.HTTP.Types.Header    (HeaderName)
import           Serv.Internal.Interpretation
import           Serv.Internal.Server.Config

data Context =
  Context
  { request         :: Wai.Request
  , pathZipper      :: ([Text], [Text])
  , headersExpected :: [(HeaderName, Maybe Text)]
  , config          :: Config

    -- cached via strictRequestBody so that we don't have to deal with multiple
    -- request body pulls affecting one another; this defeats partial and lazy body
    -- loading, BUT the style of API description we're talking about here isn't really
    -- amenable to that sort of thing anyway.
    --
    -- also note that we really need to compute this using Lazy IO; otherwise,
    -- we'll have to be handling the partial request/respond dance from the get-go.

  , body            :: S.ByteString
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
