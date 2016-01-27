{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Internal.Server.Response where

import qualified Data.ByteString.Lazy               as Sl
import           Data.Function                      ((&))
import           Data.Singletons
import           Data.Singletons.Prelude.List
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

data SomeResponse (alts :: [Alternative Nat Symbol *]) where
  StandardResponse
    :: Elem (Responding code hdrs body) alts ~ True
    => Response code hdrs body
    -> SomeResponse alts
  NonStandardResponse
    :: HTTP.Status -> [HTTP.Header] -> Maybe Sl.ByteString
    -> SomeResponse alts

-- TODO: We need more than just this proof: we need a path into the list!
-- Without this we cannot recover constraints placed on the response "from
-- the outside"

-- | While a response is constructed using other means, the response is
-- finalized here. This is essential for typing purposes alone.
respond
  :: (Elem (Responding code hdrs body) alts ~ True, Monad m)
  => Response code hdrs body -> m (SomeResponse alts)
respond = return . StandardResponse

-- | Construct a response in the event of an error. These are /not/ tracked
-- by the API type and are therefore free to return whatever they like.
respondExceptionally :: HTTP.Status -> [HTTP.Header] -> Maybe Sl.ByteString -> SomeResponse alts
respondExceptionally = NonStandardResponse

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
