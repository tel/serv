
module Serv.Server (

    Server
    , transformServer
    , makeApplication

    , Config
    , defaultConfig

      -- Implementation and constraint

    , server

    , Impl
    , Impl_Endpoint
    , Impl_OneOf
    , Impl_Path
    , Impl_Handler

    , Constrain
    , Constrain_OneOf
    , Constrain_Endpoint
    , Constrain_Handler
    , Constrain_Body
    , Constrain_Path

      -- For implementation
    , (:<|>) (..), NotFound (..), MethodNotAllowed (..)

    , Response
    , respond
    , emptyResponse
    , errorResponse
    , withBody
    , withHeader
    , withQuietHeader

      -- HTTP Status re-exports
    , module Network.HTTP.Types.Status
    , module Data.Tagged

  ) where

import           Data.Tagged
import           Network.HTTP.Types.Status
import qualified Network.Wai                   as Wai
import           Serv.Internal.Server
import           Serv.Internal.Server.Config
import           Serv.Internal.Server.Context
import           Serv.Internal.Server.Response
import           Serv.Internal.Server.Type

-- | Build a 'Wai.Application' from an implemented @'Server' 'IO'@.
makeApplication :: Config -> Server IO -> Wai.Application
makeApplication conf s = app where
  app req resp = do
    ctx <- makeContext conf req
    runServerWai ctx resp s
