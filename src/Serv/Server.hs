
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

    , Response (..)

      -- HTTP Status re-exports
    , module Network.HTTP.Types.Status


  ) where

import           Network.HTTP.Types.Status
import qualified Network.Wai                  as Wai
import           Serv.Internal.Server
import           Serv.Internal.Server.Config
import           Serv.Internal.Server.Context
import           Serv.Internal.Server.Type

makeApplication :: Config -> Server IO -> Wai.Application
makeApplication conf server = app where
  app req resp = do
    ctx <- makeContext conf req
    runServerWai ctx resp server
