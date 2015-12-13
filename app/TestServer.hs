{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

import qualified Network.Wai              as Wai
import           Network.Wai.Handler.Warp (run)
import qualified Serv.Api                 as A
import           Serv.Common
import qualified Serv.ContentType         as Ct
import           Serv.Server

type RawBody = 'A.Body '[ Ct.TextPlain ] RawText

type Api
  = 'A.Endpoint
    '[ 'A.Method 'A.GET '[] RawBody ]

impl :: Impl Api IO
impl = get :<|> noOp
  where
    get :: IO (Response '[] RawBody)
    get = return (Response ok200 Nil (RawText "Hello"))

server :: Server IO
server = handle (Proxy :: Proxy Api) impl

app :: Wai.Application
app = makeApplication defaultConfig server

main :: IO ()
main =
  run 8000 app
