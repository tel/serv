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
import           Data.Tagged
import qualified Network.HTTP.Types  as HTTP
import           Serv.Internal.HList

data Verb
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
    deriving ( Eq, Ord, Show, Read )

-- | Kind indicating the use of a type as a MIME Content Type specification.
data ContentType where
  As :: ty -> ContentType

data ResponseBody ty where
  Body :: [ContentType] -> ty -> ResponseBody ty
  NoBody :: ResponseBody ty

data Response headers body where
  ResponseWithBody
    :: HTTP.Status -> HList (HeaderImpl headers) -> a
    -> Response headers ('Body ctypes a)
  ResponseNoBody
    :: HTTP.Status -> HList (HeaderImpl headers)
    -> Response headers 'NoBody

type family HeaderImpl hs where
  HeaderImpl '[] = '[]
  HeaderImpl ( '( name, ty ) ': hs ) = Tagged name ty ': HeaderImpl hs




-- Reflection
-- ----------------------------------------------------------------------------


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

class ReflectVerb (v :: Verb) where
  reflectVerb :: Proxy v -> Verb

instance ReflectVerb 'GET where reflectVerb Proxy = GET
instance ReflectVerb 'POST where reflectVerb Proxy = POST
instance ReflectVerb 'PUT where reflectVerb Proxy = PUT
instance ReflectVerb 'PATCH where reflectVerb Proxy = PATCH
instance ReflectVerb 'DELETE where reflectVerb Proxy = DELETE
