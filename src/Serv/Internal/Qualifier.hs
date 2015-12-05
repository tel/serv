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

module Serv.Internal.Qualifier where

import           GHC.TypeLits

import           Serv.Internal.ContentType

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
