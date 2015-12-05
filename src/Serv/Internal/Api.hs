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

module Serv.Internal.Api where

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

import           Serv.Internal.Qualifier
import           Serv.Internal.Response

data Api ty where


  (:>) :: ApiQualifier ty -> Api ty -> Api ty
  OneOf :: [Api ty] -> Api ty

  Endpoint :: [Method ty] -> Api ty

  --
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
