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
module Serv.Api (

  -- * API types/kinds

    Api (..)
  , Path (..)
  , Handler (..)
  , Output (..)
  , Body (..)

  -- ** Syntax sugar
  , type (:::)

  -- * Singletons
  , Sing (
        SEndpoint, SOneOf, SAbstract, (:%>)
      , SConst, SHeaderAs, SSeg, SHeader, SWildcard
      , SOutputs, SCaptureBody, SCaptureHeaders, SCaptureQuery
      , SRespond
      , SHasBody, SEmpty
    )

  -- * Type aliases

  -- | Eliminates need for single-quoting the @DataKind@-lifted types.

  -- ** 'Api'
  , Endpoint, OneOf, Abstract, (:>)

  -- ** 'Path'
  , Const, HeaderAs, Seg, Header, Wildcard

  -- ** 'Handler'
  , Outputs, CaptureBody, CaptureHeaders, CaptureQuery

  -- ** 'Output'
  , Respond

  -- ** 'Body'
  , HasBody, Empty

) where

import           Data.Singletons
import           Data.Singletons.TypeLits
import           Network.HTTP.Kinder.Header (HeaderName)
import           Network.HTTP.Kinder.Status (Status)
import           Network.HTTP.Kinder.Verb

-- | Extra syntax-sugar for representing type-level pairs.
type a ::: b = '( a, b )
infixr 6 :::

-- | 'Handler' responses may opt to include a response body or not.
data Body star
  = HasBody [star] star
  -- ^ Return a response body by specifying a set of content-types and
  -- a value to derive the body from.
  | Empty
  -- ^ A response with an empty body

data instance Sing (b :: Body *)
  = forall ts a . b ~ HasBody ts a => SHasBody (Sing ts) (Sing a)
  | b ~ Empty => SEmpty

instance (SingI ts, SingI a) => SingI ('HasBody ts a :: Body *) where
  sing = SHasBody sing sing

instance SingI ('Empty :: Body *) where
  sing = SEmpty

type HasBody ctypes ty = 'HasBody ctypes ty
type Empty = 'Empty



-- | Describes an output from an API under a given status.
data Output star = Respond [ (HeaderName, star) ] (Body star)

data instance Sing (o :: Output *)
  = forall ts b . o ~ Respond ts b => SRespond (Sing ts) (Sing b)

instance (SingI ts, SingI b) => SingI ('Respond ts b :: Output *) where
  sing = SRespond sing sing

type Respond hdrs body = 'Respond hdrs body




-- | A 'Handler' is a single HTTP verb response handled at a given 'Endpoint'.
-- In order to complete a 'Handler''s operation it may demand data from the
-- request such as headers or the request body.
data Handler star
  = Outputs [(Status, Output star)]
    -- ^ Minimally, a 'Handler' is just a list of alternatives of outputs
    -- the server could produce, each tagged by their status code.
  | CaptureBody [star] star (Handler star)
    -- ^ Augment a 'Handler' to include requirements of a request body.
  | CaptureHeaders [(HeaderName, star)] (Handler star)
    -- ^ Augment a 'Handler' to include requirements of request header values.
  | CaptureQuery [(Symbol, star)] (Handler star)
    -- ^ Augment a 'Handler' to include requirements of the request query
    -- string

data instance Sing (h :: Handler *)
  = forall ts . h ~ Outputs ts => SOutputs (Sing ts)
  | forall ts a k . h ~ CaptureBody ts a k => SCaptureBody (Sing ts) (Sing a) (Sing k)
  | forall ts k . h ~ CaptureHeaders ts k => SCaptureHeaders (Sing ts) (Sing k)
  | forall ts k . h ~ CaptureQuery ts k => SCaptureQuery (Sing ts) (Sing k)

instance (SingI ts) => SingI ('Outputs ts :: Handler *) where
  sing = SOutputs sing

instance (SingI ts, SingI a, SingI k) => SingI ('CaptureBody ts a k :: Handler *) where
  sing = SCaptureBody sing sing sing

instance (SingI ts, SingI k) => SingI ('CaptureHeaders ts k :: Handler *) where
  sing = SCaptureHeaders sing sing

instance (SingI ts, SingI k) => SingI ('CaptureQuery ts k :: Handler *) where
  sing = SCaptureQuery sing sing

type Outputs responses = 'Outputs responses
type CaptureBody cTypes ty handler = 'CaptureBody cTypes ty handler
type CaptureHeaders hdrs handler = 'CaptureHeaders hdrs handler
type CaptureQuery query handler = 'CaptureQuery query handler




-- | "Generalized" path segments match against data in the request.
data Path star
  = Const Symbol
    -- ^ Matches if the request has a non-empty remaining path and the next
    -- segment matches exactly
  | HeaderAs HeaderName Symbol
    -- ^ Matches if the request has a given header and its value matches
    -- exactly (!)
  | Seg Symbol star
    -- ^ Matches if the request has a non-empty remaining path. The next
    -- segment is "captured", provided to the server implementation.
  | Header HeaderName star
    -- ^ Always matches, "capturing" the value of a header, or 'Nothing' if
    -- the header fails to exist.
  | Wildcard
    -- ^ Always matches, "captures" the remaining path segments as a list
    -- of text values. May just capture the empty list.

data instance Sing (p :: Path *)
  = forall s . p ~ Const s => SConst (Sing s)
  | forall n v . p ~ HeaderAs n v => SHeaderAs (Sing n) (Sing v)
  | forall n t . p ~ Seg n t => SSeg (Sing n) (Sing t)
  | forall n t . p ~ Header n t => SHeader (Sing n) (Sing t)
  | p ~ Wildcard => SWildcard

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

type Const sym = 'Const sym
type HeaderAs ty sym = 'HeaderAs ty sym
type Seg sym ty = 'Seg sym ty
type Header name ty = 'Header name ty
type Wildcard = 'Wildcard




-- | 'Api's describe collections of HTTP endpoints accessible at
-- various segmented 'Path's.
data Api star
  = Endpoint star [(Verb, Handler star)]
    -- ^ An 'Endpoint' describes a root API which responds only to requests
    -- with empty paths. It matches on HTTP 'Verb's, 'HeaderName's, and
    -- 'Body's.
    --
    -- 'Endpoint' differs from 'OneOf' in that it can only choose between
    -- possible methods and automatically provides an 'OPTIONS' response.
  | OneOf [Api star]
    -- ^ 'Api's consist of many sub-'Api's which are attempted sequentially.
    -- @'OneOf' choices@ expresses this sequential search along a set of
    -- sub-'Api' @choices@.
  | Abstract
    -- ^ 'Abstract' enables the use of standard 'Wai.Application's within an
    -- 'Api'. These cannot be examined further through type analysis, but
    -- they are a common use case.
  | Path star :> Api star
    -- ^ Qualify an API using a series of 'Path' "segments"
infixr 5 :>

data instance Sing (a :: Api *)
  = forall t ts . a ~ Endpoint t ts => SEndpoint (Sing t) (Sing ts)
  | forall ts . a ~ OneOf ts => SOneOf (Sing ts)
  | a ~ Abstract => SAbstract
  | forall p k . a ~ (p :> k) => Sing p :%> Sing k

instance (SingI t, SingI ts) => SingI ('Endpoint t ts :: Api *) where
  sing = SEndpoint sing sing

instance SingI ts => SingI ('OneOf ts :: Api *) where
  sing = SOneOf sing

instance SingI (Abstract :: Api *) where
  sing = SAbstract

instance (SingI p, SingI k) => SingI (p :> k :: Api *) where
  sing = sing :%> sing

type Endpoint ann ms = 'Endpoint ann ms
type OneOf apis = 'OneOf apis
type Abstract = 'Abstract
type a :> b = a ':> b

