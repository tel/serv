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

module Serv.Internal.Response where

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

import           Serv.Internal.HList
import           Serv.Internal.Interpretation
import           Serv.Internal.ContentType

data Verb
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE

data ResponseHeader ty = ResponseHeader Symbol ty

data ResponseBody ty where
  Body :: [ContentType] -> ty -> ResponseBody ty
  NoBody :: ResponseBody ty

data Method ty where
  Method :: Verb -> [ResponseHeader ty] -> ResponseBody ty -> Method ty

data Response headers body where
  ResponseWithBody
    :: HTTP.Status -> HList (HeaderImpl headers) -> a
    -> Response headers ('Body ctypes a)
  ResponseNoBody
    :: HTTP.Status -> HList (HeaderImpl headers)
    -> Response headers 'NoBody

type family HeaderImpl hs where
  HeaderImpl '[] = '[]
  HeaderImpl ('ResponseHeader sym ty ': hs) = Tagged sym ty ': HeaderImpl hs




-- Reflection
-- ----------------------------------------------------------------------------

reflectHeader :: forall a sym . (HeaderEncode a, KnownSymbol sym) => Tagged sym a -> HTTP.Header
reflectHeader (Tagged v) =
  let headerName = fromString (symbolVal (Proxy :: Proxy sym))
      headerValue = Text.encodeUtf8 (headerEncode v)
  in (headerName, headerValue)

-- class ReflectHeaders ls where
--   reflectHeaders :: HList ls -> [HTTP.Header]

-- instance ReflectHeaders '[] where
--   reflectHeaders HNil = []

-- instance
--   (ReflectHeaders ls, HeaderEncode a, KnownSymbol sym) =>
--     ReflectHeaders ('ResponseHeader sym a ': ls)
--   where
--     reflectHeaders (HCons h hs) =
--       reflectHeader h : reflectHeaders hs

-- class WaiResponse body where
--   waiResponse :: ReflectHeaders headers => Response headers body -> Wai.Response

-- instance WaiResponse 'NoBody where
--   waiResponse (ResponseNoBody status headers) =
--     Wai.responseLBS status (reflectHeaders headers) ""

class ReflectVerbs methods where
  reflectVerbs :: Proxy methods -> [Verb]

instance ReflectVerbs '[] where
  reflectVerbs Proxy = []

instance ReflectVerbs methods => ReflectVerbs ('Method 'GET hdrs body ': methods) where
  reflectVerbs Proxy = GET : reflectVerbs (Proxy :: Proxy methods)

instance ReflectVerbs methods => ReflectVerbs ('Method 'POST hdrs body ': methods) where
  reflectVerbs Proxy = POST : reflectVerbs (Proxy :: Proxy methods)

instance ReflectVerbs methods => ReflectVerbs ('Method 'PUT hdrs body ': methods) where
  reflectVerbs Proxy = PUT : reflectVerbs (Proxy :: Proxy methods)

instance ReflectVerbs methods => ReflectVerbs ('Method 'PATCH hdrs body ': methods) where
  reflectVerbs Proxy = PATCH : reflectVerbs (Proxy :: Proxy methods)

instance ReflectVerbs methods => ReflectVerbs ('Method 'DELETE hdrs body ': methods) where
  reflectVerbs Proxy = DELETE : reflectVerbs (Proxy :: Proxy methods)
