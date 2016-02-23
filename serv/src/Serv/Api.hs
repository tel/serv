{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types, but really kinds, which represent the structure of an API.
module Serv.Api where

import           Data.Singletons
import           Data.Singletons.TypeLits
import           Network.HTTP.Kinder.Header (HeaderName)
import           Network.HTTP.Kinder.Status (Status)
import           Network.HTTP.Kinder.Verb

-- | Extra syntax-sugar for representing type-level pairs.
type a ::: b = '( a, b )
infixr 6 :::

-- | 'Handler' responses may opt to include a response body or not.
--
-- Return a response body by specifying a set of content-types
-- and a value to derive the body from.
--
-- A response with an empty body
data Body star where
  HasBody :: [star] -> star -> Body star
  Empty :: Body star

data instance Sing (b :: Body *)
  = forall ts a . b ~ HasBody ts a => SHasBody (Sing ts) (Sing a)
  | b ~ Empty => SEmpty

instance (SingI ts, SingI a) => SingI ('HasBody ts a :: Body *) where
  sing = SHasBody sing sing

instance SingI ('Empty :: Body *) where
  sing = SEmpty

type HasBody ctypes ty = 'HasBody ctypes ty
type Empty = 'Empty




data Output star where
  Respond :: [ (HeaderName, star) ] -> Body star -> Output star

data instance Sing (o :: Output *)
  = forall ts b . o ~ Respond ts b => SRespond (Sing ts) (Sing b)

instance (SingI ts, SingI b) => SingI ('Respond ts b :: Output *) where
  sing = SRespond sing sing

type Respond hdrs body = 'Respond hdrs body




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
data Handler star where
  Method :: Verb -> [ (Status, Output star) ] -> Handler star
  CaptureBody :: [star] -> star -> Handler star -> Handler star
  CaptureHeaders :: [ (HeaderName, star) ] -> Handler star -> Handler star
  CaptureQuery :: [ (Symbol, star) ] -> Handler star -> Handler star

data instance Sing (h :: Handler *)
  = forall v ts . h ~ Method v ts => SMethod (Sing v) (Sing ts)
  | forall ts a k . h ~ CaptureBody ts a k => SCaptureBody (Sing ts) (Sing a) (Sing k)
  | forall ts k . h ~ CaptureHeaders ts k => SCaptureHeaders (Sing ts) (Sing k)
  | forall ts k . h ~ CaptureQuery ts k => SCaptureQuery (Sing ts) (Sing k)

instance (SingI v, SingI ts) => SingI ('Method v ts :: Handler *) where
  sing = SMethod sing sing

instance (SingI ts, SingI a, SingI k) => SingI ('CaptureBody ts a k :: Handler *) where
  sing = SCaptureBody sing sing sing

instance (SingI ts, SingI k) => SingI ('CaptureHeaders ts k :: Handler *) where
  sing = SCaptureHeaders sing sing

instance (SingI ts, SingI k) => SingI ('CaptureQuery ts k :: Handler *) where
  sing = SCaptureQuery sing sing

type Method verb responses = 'Method verb responses
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

data Path star where
  Const :: Symbol -> Path star
  HeaderAs :: HeaderName -> Symbol -> Path star
  Seg :: Symbol -> star -> Path star
  Header :: HeaderName -> star -> Path star
  Wildcard :: Path star
  Cors :: star -> Path star

data instance Sing (p :: Path *)
  = forall s . p ~ Const s => SConst (Sing s)
  | forall n v . p ~ HeaderAs n v => SHeaderAs (Sing n) (Sing v)
  | forall n t . p ~ Seg n t => SSeg (Sing n) (Sing t)
  | forall n t . p ~ Header n t => SHeader (Sing n) (Sing t)
  | p ~ Wildcard => SWildcard
  | forall t . p ~ Cors t => SCors (Sing t)

instance SingI s => SingI (Const s :: Path *) where
  sing = SConst sing

instance (SingI n, SingI v) => SingI (HeaderAs n v :: Path *) where
  sing = SHeaderAs sing sing

instance (SingI n, SingI t) => SingI (Seg n t :: Path *) where
  sing = SSeg sing sing

instance (SingI n, SingI t) => SingI (Header n t :: Path *) where
  sing = SHeader sing sing

instance SingI (Wildcard :: Path *) where
  sing = SWildcard

instance SingI t => SingI (Cors t :: Path *) where
  sing = SCors sing

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
data Api star where
  Endpoint :: star -> [Handler star] -> Api star
  OneOf :: [Api star] -> Api star
  Raw :: Api star
  (:>) :: Path star -> Api star -> Api star
infixr 5 :>

data instance Sing (a :: Api *)
  = forall t ts . a ~ Endpoint t ts => SEndpoint (Sing t) (Sing ts)
  | forall ts . a ~ OneOf ts => SOneOf (Sing ts)
  | a ~ Raw => SRaw
  | forall p k . a ~ (p :> k) => Sing p :%> Sing k

instance (SingI t, SingI ts) => SingI ('Endpoint t ts :: Api *) where
  sing = SEndpoint sing sing

instance SingI ts => SingI ('OneOf ts :: Api *) where
  sing = SOneOf sing

instance SingI (Raw :: Api *) where
  sing = SRaw

instance (SingI p, SingI k) => SingI (p :> k :: Api *) where
  sing = sing :%> sing

type Endpoint ann ms = 'Endpoint ann ms
type OneOf apis = 'OneOf apis
type Raw = 'Raw
type a :> b = a ':> b

