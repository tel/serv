{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators         #-}

import           Data.Function            ((&))
import           Data.Proxy
import           Network.Wai.Handler.Warp (run)
import qualified Serv.Api                 as A
import           Serv.Common
import qualified Serv.ContentType         as Ct
import qualified Serv.Header              as H
import qualified Serv.Header.Proxies      as Hp
import           Serv.Server

type RawBody = 'A.Body '[ Ct.TextPlain ] Text

type Api
  = 'A.Endpoint
    '[ 'A.Method 'A.GET '[ 'H.CacheControl 'A.::: RawText ] RawBody ]

apiProxy :: Proxy Api
apiProxy = Proxy

impl :: Impl Api IO
impl = get :<|> noOp
  where
    get =
      return
      $ emptyResponse ok200
      & withHeader Hp.cacheControl "foo"
      & withBody "Hello"

server :: Server IO
server = handle apiProxy impl

main :: IO ()
main =
  run 8000 (makeApplication defaultConfig server)
