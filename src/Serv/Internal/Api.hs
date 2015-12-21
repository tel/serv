{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types, but really kinds, which represent the structure of an API.
module Serv.Internal.Api where

import           Data.Singletons.TH
import           GHC.TypeLits
import           Serv.Internal.Header (HeaderType)
import           Serv.Internal.Pair
import           Serv.Internal.Verb

-- | 'Handler' responses may opt to include a response body or not.
--
-- Return a response body by specifying a set of content-types
-- and a value to derive the body from.
--
-- A response with an empty body
singletons
  [d|
    data Body star where
      HasBody :: [star] -> star -> Body star
      Empty :: Body star
  |]

type HasBody ctypes ty = 'HasBody ctypes ty
type Empty = 'Empty

-- | A 'Handler' is a single HTTP verb response handled at a given 'Endpoint'.
-- In order to complete a 'Handler''s operation it may demand data from the
-- request such as headers or the request body.
--
-- A "core" 'Handler' definition which describes the 'Verb' it responds
-- to along with a set of response headers and a chance to attach a
-- response 'Body'.
--
-- Augment a 'Handler' to include requirements of a request body.
--
-- Augment a 'Handler' to include requirements of request header values.
--
singletons
  [d|
    data Handler symbol star where
      Method :: Verb -> [ (HeaderType symbol, star) ] -> Body star -> Handler symbol star
      CaptureBody :: [star] -> star -> Handler symbol star -> Handler symbol star
      CaptureHeaders :: [ (HeaderType symbol, star) ] -> Handler symbol star -> Handler symbol star
      CaptureQuery :: [ (symbol, star) ] -> Handler symbol star -> Handler symbol star
  |]

type Method verb responseHeaders body = 'Method verb responseHeaders body
type CaptureBody cTypes ty method = 'CaptureBody cTypes ty method
type CaptureHeaders hdrs method = 'CaptureHeaders hdrs method
type CaptureQuery query method = 'CaptureQuery query method

-- | "Generalized" path segments match against data in the request.
--
-- Matches if the request has a non-empty remaining path and
-- the next segment matches exactly
--
-- Matches if the request has a given header and its value
-- matches exactly (!)
--
-- Matches if the request has a non-empty remaining path.
-- The next segment is "captured", provided to the server implementation.
--
-- Always matches, "capturing" the value of a header, or 'Nothing' if
-- the header fails to exist.
--
-- Always matches, "captures" the remaining path segments as a list
-- of text values. May just capture the empty list.

singletons
  [d|
    data Path symbol star where
      Const :: symbol -> Path symbol star
      HeaderAs :: HeaderType symbol -> symbol -> Path symbol star
      Seg :: symbol -> star -> Path symbol star
      Header :: HeaderType symbol -> star -> Path symbol star
      Wildcard :: Path symbol star
      Cors :: star -> Path symbol star
  |]

type Const sym = 'Const sym
type HeaderAs ty sym = 'HeaderAs ty sym
type Seg sym ty = 'Seg sym ty
type Header name ty = 'Header name ty
type Wildcard = 'Wildcard
type Cors ty = 'Cors ty

-- | 'Api's describe collections of HTTP endpoints accessible at
-- various segmented 'Path's.
--
-- An 'Endpoint' describes a root API which responds
-- only to requests with empty paths. It matches on HTTP 'Method's
-- which demand 'Verb's, 'HeaderName's, and 'Body's.
--
-- 'Endpoint' differs from 'OneOf' in that it can only choose between
-- possible methods and automatically provides an 'OPTIONS' response.
--
-- 'Api's consist of many sub-'Api's which are attempted sequentially.
-- @'OneOf' choices@ expresses this sequential search along a set of sub-'Api'
-- @choices@.
--
-- 'Raw' enables the use of standard 'Wai.Application's within an 'Api'.
-- These cannot be examined further through type analysis, but they are a
-- common use case.
--
-- Qualify an API using a series of 'Path' qualifiers.

singletons
  [d|
    data Api symbol star where
      Endpoint :: star -> [Handler symbol star] -> Api symbol star
      OneOf :: [Api symbol star] -> Api symbol star
      Raw :: Api symbol star
      (:>) :: Path symbol star -> Api symbol star -> Api symbol star
  |]

type Endpoint ann ms = 'Endpoint ann ms
type OneOf apis = 'OneOf apis
type Raw = 'Raw
type a :> b = a ':> b

infixr 5 :>
