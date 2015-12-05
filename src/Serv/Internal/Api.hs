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

module Serv.Internal.Api where

import GHC.TypeLits
import           Serv.Internal.Response

data Api ty where
  (:>) :: ApiQualifier ty -> Api ty -> Api ty
  OneOf :: [Api ty] -> Api ty

  Endpoint :: [Method ty] -> Api ty

  --   Raw :: Api ty
  --
  --
  --
  -- To achieve this (or an UPGRADE endpoint) we need to augment Server m to
  -- internalize Wai.Application values. Something like
  --
  --   Server m ~ Context -> Either3T RoutingErr Wai.Application m Wai.Result
  --
  -- indicating that instead of returning a result sometimes we "upgrade" to a
  -- whole new Wai.Application which will from here on out handle the request.
  --
  -- The previous design ought to be easy to transform into a Wai.Application
  --
  --   Server IO -> Wai.Application
  --
  -- since it can just "hook in" the new application when needed.

data ApiQualifier ty where
  Seg :: Symbol -> ApiQualifier ty
  MatchHeader :: Symbol -> Symbol -> ApiQualifier ty

  CaptureSeg :: Symbol -> ty -> ApiQualifier ty
  CaptureHeader :: Symbol -> ty -> ApiQualifier ty
  CaptureBody :: [ContentType] -> ty -> ApiQualifier ty
  CaptureContext :: ApiQualifier ty

  -- These two are elided for the moment as we're not using query strings yet;
  -- they're not terrifically hard to implement, though.
  --
  --
  -- CaptureFlag :: Symbol -> Api ty
  -- CaptureParam :: Symbol -> ty -> Api ty
