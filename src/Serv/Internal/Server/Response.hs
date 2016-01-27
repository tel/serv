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
import           Serv.Internal.Rec                  (Rec)
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

respond
  :: (Elem (Responding code hdrs body) alts ~ True, Monad m)
  => Response code hdrs body -> m (SomeResponse alts)
respond = return . StandardResponse

-- -- An 'emptyResponse' returns the provided status message with no body or headers
-- emptyResponse :: HTTP.Status -> Response Ok '[] 'Empty
-- emptyResponse status = EmptyResponse status [] Nil
--
-- -- | Adds a body to a response
-- withBody
--   :: a -> Response Ok headers 'Empty -> Response Ok headers ('HasBody ctypes a)
-- withBody a (EmptyResponse status secretHeaders headers) =
--   Response status secretHeaders headers a
--
-- -- | Adds a header to a response
-- withHeader
--   :: Sing name -> value
--   -> Response Ok headers body -> Response Ok (name ::: value ': headers) body
-- withHeader s val r = case r of
--   Response status secretHeaders headers body ->
--     Response status secretHeaders (headers & s -: val) body
--   EmptyResponse status secretHeaders headers ->
--     EmptyResponse status secretHeaders (headers & s -: val)
--
-- -- | Unlike 'withHeader', 'withQuietHeader' allows you to add headers
-- -- not explicitly specified in the api specification.
-- withQuietHeader
--   :: HeaderEncode name value
--      => Sing name -> value
--      -> Response e headers body -> Response e headers body
-- withQuietHeader s value r =
--   case headerPair s value of
--     Nothing -> r
--     Just newHeader ->
--       case r of
--         Response status secretHeaders headers body ->
--           Response status (newHeader : secretHeaders) headers body
--         EmptyResponse status secretHeaders headers ->
--           EmptyResponse status (newHeader : secretHeaders) headers
--         ErrorResponse status headers body ->
--           ErrorResponse status (newHeader : headers) body
--
-- -- | Construct a response in the event of an error. These are /not/ tracked
-- -- by the API type and are therefore free to return whatever they like.
-- errorResponse :: HTTP.Status -> [HTTP.Header] -> Maybe Sl.ByteString -> Response NonStandard h b
-- errorResponse = ErrorResponse
--
-- -- | If a response type is complete defined by its implementation then
-- -- applying 'resorted' to it will future proof it against reorderings
-- -- of headers. If the response type is not completely inferrable, however,
-- -- then this will require manual annotations of the "pre-sorted" response.
-- resortHeaders :: RecordIso headers headers' => Response e headers body -> Response e headers' body
-- resortHeaders r =
--   case r of
--     Response status secretHeaders headers body ->
--       Response status secretHeaders (reorder headers) body
--     EmptyResponse status secretHeaders headers ->
--       EmptyResponse status secretHeaders (reorder headers)
--     ErrorResponse s h b -> ErrorResponse s h b
--
-- -- | Used primarily for implementing @HEAD@ request automatically.
-- deleteBody :: Response e headers body -> Response e headers 'Empty
-- deleteBody r =
--   case r of
--     Response status secretHeaders headers _ ->
--       EmptyResponse status secretHeaders headers
--     EmptyResponse{} -> r
--     ErrorResponse s h _ -> ErrorResponse s h Nothing
