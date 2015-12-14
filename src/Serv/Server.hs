
module Serv.Server (

    Server
    , transformServer
    , makeApplication

    , Config
    , defaultConfig

    , Handling (handle)
    , Impl

      -- For implementation
    , (:<|>) (..), NotHere (..)
    , noOp

    , Response
    , emptyResponse
    , withBody
    , withHeader
    , withQuietHeader

      -- HTTP Status re-exports
    , module Network.HTTP.Types.Status
    , module Data.Tagged


  ) where

import           Data.Tagged
import           Network.HTTP.Types.Status
import qualified Network.Wai                  as Wai
import           Serv.Internal.Server
import           Serv.Internal.Server.Config
import           Serv.Internal.Server.Context
import           Serv.Internal.Server.Type

-- | Build a 'Wai.Application' from an implemented @'Server' 'IO'@.
makeApplication :: Config -> Server IO -> Wai.Application
makeApplication conf server = app where
  app req resp = do
    ctx <- makeContext conf req
    runServerWai ctx resp server
