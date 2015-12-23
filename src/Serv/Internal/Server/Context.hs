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

module Serv.Internal.Server.Context where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString                    as S
import qualified Data.ByteString.Lazy               as Sl
import qualified Data.IORef                         as IORef
import           Data.Monoid
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Singletons
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8')
import           GHC.TypeLits
import qualified Network.HTTP.Types                 as HTTP
import qualified Network.Wai                        as Wai
import           Serv.Internal.Api
import qualified Serv.Internal.Api.Analysis         as Analysis
import qualified Serv.Internal.Cors                 as Cors
import qualified Serv.Internal.Header               as Header
import qualified Serv.Internal.Header.Serialization as HeaderS
import           Serv.Internal.RawText
import           Serv.Internal.Server.Config
import qualified Serv.Internal.URI                  as URI
import qualified Data.CaseInsensitive as CI

data Context =
  Context
  { request         :: Wai.Request
  , pathZipper      :: ([Text], [Text])
  , headersExpected :: [(HTTP.HeaderName, Maybe Text)]
  , config          :: Config

    -- cached via strictRequestBody so that we don't have to deal with multiple
    -- request body pulls affecting one another; this defeats partial and lazy body
    -- loading, BUT the style of API description we're talking about here isn't really
    -- amenable to that sort of thing anyway.
    --
    -- also note that we really need to compute this using Lazy IO; otherwise,
    -- we'll have to be handling the partial request/respond dance from the get-go.

  , body            :: S.ByteString

  , corsPolicies    :: [Cors.Policy]
  }

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

corsHeaders
  :: forall (hs :: [Handler Symbol *])
  . Sing hs -> Cors.IncludeMethods -> Context -> Maybe [HTTP.Header]
corsHeaders s includeMethods ctx = do
  let ana = Analysis.inspectEndpoint s
      policyChain = corsPolicies ctx
      conf = config ctx
  RawText origin <- examineHeaderFast Header.SOrigin ctx
  let corsContext =
        Cors.Context
        { Cors.origin = origin
        , Cors.headersExpected =
            Analysis.headersExpected ana
            <>
            requestHeadersSeen ctx
        , Cors.headersReturned =
            Analysis.headersEmitted ana
        , Cors.methodsAvailable =
            Analysis.verbsHandled ana
        }
  let accessSet = foldMap (\p -> p conf corsContext) policyChain
  return (Cors.headerSet includeMethods corsContext accessSet)

makeContext :: Config -> Wai.Request -> IO Context
makeContext theConfig theRequest = do
  theBody <- Wai.strictRequestBody theRequest
  -- We create a "frozen", strict version of the body and augment the request to
  -- always return it directly.
  ref <- IORef.newIORef (Sl.toStrict theBody)
  return Context { request = theRequest { Wai.requestBody = IORef.readIORef ref }
                 , config = theConfig
                 , body = Sl.toStrict theBody
                 , pathZipper = ([], Wai.pathInfo theRequest)
                 , headersExpected = []
                 , corsPolicies = []
                 }

pathIsEmpty :: Context -> Bool
pathIsEmpty ctx = case pathZipper ctx of
  (_, []) -> True
  _ -> False

method :: Context -> HTTP.Method
method = Wai.requestMethod . request

requestHeadersSeen :: Context -> Set (Header.HeaderType Text)
requestHeadersSeen ctx =
  Set.fromList (
    mapMaybe
      (fmap (Header.nameHeader . CI.mk) . hush . decodeUtf8' . CI.original . fst)
      (headersExpected ctx)
  )

-- | Pop all remaining segments off the context
takeAllSegments :: Context -> (Context, [Text])
takeAllSegments ctx = (newContext, fore) where
  newContext = ctx { pathZipper = (reverse fore ++ hind, []) }
  (hind, fore) = pathZipper ctx

-- | Pop a segment off the URI and produce a new context for "beyond" that segment
takeSegment :: Context -> (Context, Maybe Text)
takeSegment ctx = (stepContext ctx, safeHead fore) where
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
pullHeaderRaw :: HTTP.HeaderName -> Context -> (Context, Maybe S.ByteString)
pullHeaderRaw name ctx =
  (newContext, lookup name headers)
  where
    newContext = ctx { headersExpected = (name, Nothing) : headersExpected ctx }
    headers = Wai.requestHeaders req
    req = request ctx

-- | Pull a header value from the context, updating it to note that we looked
examineHeader
  :: HeaderS.HeaderDecode n a
     => Sing n -> Context -> (Context, Either String a)
examineHeader s ctx =
  (newContext, HeaderS.headerDecodeRaw s rawString)
  where
    headerName = Header.headerName (Header.headerType s)
    (newContext, rawString) = pullHeaderRaw headerName ctx

-- | Sort of like 'examineHeader' but used for when we just want the value
-- and don't care about updating the context or worrying about
-- distinguishing between decoding failure and outright not being there at
-- all!
examineHeaderFast :: HeaderS.HeaderDecode n a => Sing n -> Context -> Maybe a
examineHeaderFast s ctx =
  let (_, hdr) = pullHeaderRaw (Header.headerName (Header.headerType s)) ctx
  in hush (HeaderS.headerDecodeRaw s hdr)

-- | Match a header value in the context, updating it to show that we looked
expectHeader :: forall (n :: Header.HeaderType Symbol) . Sing n -> Text -> Context -> (Context, Bool)
expectHeader s value ctx =
  (newContext, valOk)

  where
    valOk =
      case fmap URI.fromByteString mayVal of
        Nothing -> False
        Just (Left _) -> False
        Just (Right (RawText observation)) -> observation == value

    headerName = Header.headerName (Header.headerType s)
    mayVal = lookup headerName headers
    newContext = ctx { headersExpected = (headerName, Just value) : headersExpected ctx }
    headers = Wai.requestHeaders req
    req = request ctx

-- Util
-- ----------------------------------------------------------------------------

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a
