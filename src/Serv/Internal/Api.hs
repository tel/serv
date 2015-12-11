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

-- | Types, but really kinds, which represent the structure of an API.
module Serv.Internal.Api where

import           GHC.TypeLits
import           Serv.Internal.Header (HeaderName)
import           Serv.Internal.Pair
import           Serv.Internal.Verb


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
data Api star where
  Endpoint :: [Method star] -> Api star
  OneOf :: [Api star] -> Api star
  Raw :: Api star
  (:>) :: Path star -> Api star -> Api star

-- | A 'Method' is a single HTTP verb response handled at a given 'Endpoint'.
-- In order to complete a 'Method''s operation it may demand data from the
-- request such as headers or the request body.
--
-- A "core" 'Method' definition which describes the 'Verb' it responds
-- to along with a set of response headers and a chance to attach a
-- response 'Body'.
--
-- Augment a 'Method' to include requirements of a request body.
--
-- Augment a 'Method' to include requirements of request header values.
--
data Method star where
  Method :: Verb -> [Pair HeaderName star] -> Body star -> Method star
  CaptureBody :: [star] -> star -> Method star -> Method star
  CaptureHeaders :: [Pair HeaderName star] -> Method star -> Method star
  CaptureQuery :: [Pair Symbol star] -> Method star -> Method star

-- | 'Method' responses may opt to include a response body or not.
--
-- Return a response body by specifying a set of content-types
-- and a value to derive the body from.
--
-- A response with an empty body
data Body star where
  Body :: [star] -> star -> Body star
  Empty :: Body star

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
--
-- Always matches, "captures" the existence of a query flag by
-- returning 'True' if the flag is provided and 'False' otherwise.
--
-- Always matches, "capturing" the value of a query parameter.
data Path star where
  Const :: Symbol -> Path star
  HeaderAs :: HeaderName -> Symbol -> Path star
  Seg :: Symbol -> star -> Path star
  Header :: HeaderName -> star -> Path star
  Wildcard :: Path star
  Flag :: Symbol -> Path star
  QueryParam :: Symbol -> star -> Path star
