{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Serv.Internal.Server.Response where

import           Data.Function                      ((&))
import           Data.Singletons
import           GHC.TypeLits
import qualified Network.HTTP.Types                 as HTTP
import           Serv.Internal.Api
import           Serv.Internal.Header
import           Serv.Internal.Header.Serialization
import           Serv.Internal.Pair
import           Serv.Internal.Rec                  (Rec (Nil), (-:))
import qualified Serv.Internal.Rec                  as Rec
import           Serv.Internal.StatusCode           (StatusCode)

-- | Responses generated in 'Server' implementations.
data Response (status :: StatusCode Nat) (headers :: [ (HeaderType Symbol, *) ]) body where
  ContentResponse
    :: [HTTP.Header] -> Rec headers -> a
    -> Response status headers ('HasBody ctypes a)
  EmptyResponse
    :: [HTTP.Header] -> Rec headers
    -> Response status headers 'Empty

data SomeResponse (alts :: [ (StatusCode Nat, Output Symbol *) ]) where
  SkipResponse
    :: SomeResponse alts -> SomeResponse (code ::: Respond hdrs body ': alts)
  StandardResponse
    :: Response code hdrs body
    -> SomeResponse (code ::: Respond hdrs body ': alts)

class ValidResponse alts status headers body where
  injectResponse :: Response status headers body -> SomeResponse alts

-- Here we lift headers and body matching up into type equalities instead
-- of trying to dispatch them immediately. This is very intentional!
-- Without this we'll fail to match this instance as often as we like since
-- when body or headers are ambiguous the instance matching will miss. By
-- promoting them to equalities we will find the instances based off the
-- status, as desired, then try to unify the other two later.
instance
    {-# OVERLAPS #-} (headers' ~ headers, body' ~ body) => ValidResponse
    (status ::: Respond headers' body' ': alts) status headers body where
  injectResponse = StandardResponse

instance
    ValidResponse alts status headers body =>
    ValidResponse (status' ::: Respond headers' body' ': alts) status headers body where
  injectResponse = SkipResponse . injectResponse

-- | While a response is constructed using other means, the response is
-- finalized here. This is essential for typing purposes alone.
respond
  :: (ValidResponse alts code hdrs body, Monad m)
  => Response code hdrs body -> m (SomeResponse alts)
respond = return . injectResponse

-- An 'emptyResponse' returns the provided status message with no body or headers
emptyResponse :: Sing c -> Response c '[] 'Empty
emptyResponse _status = emptyResponse'

-- An 'emptyResponse'' returns the provided status message with no body or headers
emptyResponse' :: Response c '[] 'Empty
emptyResponse' = EmptyResponse [] Nil

-- | Adds a body to a response
withBody
  :: a -> Response s headers 'Empty -> Response s headers ('HasBody ctypes a)
withBody a (EmptyResponse secretHeaders headers) =
  ContentResponse secretHeaders headers a

-- | Adds a header to a response
withHeader
  :: Sing name -> value
  -> Response s headers body -> Response s (name ::: value ': headers) body
withHeader s val r = case r of
  ContentResponse secretHeaders headers body ->
    ContentResponse secretHeaders (headers & s -: val) body
  EmptyResponse secretHeaders headers ->
    EmptyResponse secretHeaders (headers & s -: val)

-- | Unlike 'withHeader', 'withQuietHeader' allows you to add headers
-- not explicitly specified in the api specification.
withQuietHeader
  :: HeaderEncode name value
     => Sing name -> value
     -> Response e headers body -> Response e headers body
withQuietHeader s value r =
  case headerPair s value of
    Nothing -> r
    Just newHeader ->
      case r of
        ContentResponse secretHeaders headers body ->
          ContentResponse (newHeader : secretHeaders) headers body
        EmptyResponse secretHeaders headers ->
          EmptyResponse (newHeader : secretHeaders) headers

-- | If a response type is complete defined by its implementation then
-- applying 'resorted' to it will future proof it against reorderings
-- of headers. If the response type is not completely inferrable, however,
-- then this will require manual annotations of the "pre-sorted" response.
resortHeaders :: Rec.RecordIso headers headers' => Response e headers body -> Response e headers' body
resortHeaders r =
  case r of
    ContentResponse secretHeaders headers body ->
      ContentResponse secretHeaders (Rec.reorder headers) body
    EmptyResponse secretHeaders headers ->
      EmptyResponse secretHeaders (Rec.reorder headers)

-- | Used primarily for implementing @HEAD@ request automatically.
deleteBody :: Response e headers body -> Response e headers 'Empty
deleteBody r =
  case r of
    ContentResponse secretHeaders headers _ ->
      EmptyResponse secretHeaders headers
    EmptyResponse{} -> r
