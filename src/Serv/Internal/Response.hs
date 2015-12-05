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

module Serv.Internal.Response where

import           Data.Proxy
import           Data.String
import           Data.Tagged
import qualified Data.Text.Encoding           as Text
import           GHC.TypeLits
import qualified Network.HTTP.Types           as HTTP

import           Serv.Internal.ContentType
import           Serv.Internal.HList
import           Serv.Internal.Interpretation

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
