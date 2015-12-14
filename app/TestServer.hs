{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators         #-}

import           Data.Function            ((&))
import           Data.Proxy
import qualified Network.Wai              as Wai
import           Network.Wai.Handler.Warp (run)
import qualified Serv.Api                 as A
import qualified Serv.ContentType         as Ct
import qualified Serv.Header              as H
import qualified Serv.Header.Proxies      as Hp
import           Serv.Server

type RawBody = 'A.Body '[ Ct.TextPlain ] Ct.RawText

type Api
  = 'A.Endpoint
    '[ 'A.Method 'A.GET '[ 'H.CacheControl 'A.::: Ct.RawText ] RawBody ]

impl :: Impl Api IO
impl = get :<|> noOp
  where
    get =
      return
      $ withBody "Hello"
      . withHeader Hp.cacheControl "foo"
      $ emptyResponse ok200

server :: Server IO
server = handle (Proxy :: Proxy Api) impl

app :: Wai.Application
app = makeApplication defaultConfig server

main :: IO ()
main =
  run 8000 app
