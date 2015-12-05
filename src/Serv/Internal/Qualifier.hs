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
