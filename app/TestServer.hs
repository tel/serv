{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

import           Data.Text.Encoding       (encodeUtf8)
import qualified Network.Wai              as Wai
import           Network.Wai.Handler.Warp (run)
import qualified Serv.Api                 as A
import           Serv.Common
import qualified Serv.ContentType         as Ct
import           Serv.Server

data TextPlain

instance Ct.HasMediaType TextPlain where
  mediaType _ = "text" Ct.// "plain"

instance Ct.MimeEncode TextPlain RawText where
  mimeEncode _ (RawText t) = encodeUtf8 t

type Api
  = 'A.Endpoint
    '[ 'A.Method 'A.GET '[] ('A.Body '[ TextPlain ] RawText) ]

impl :: Impl Api IO
impl = get :<|> noOp
  where
    get :: IO (Response '[] ('A.Body '[ TextPlain ] RawText))
    get = return (Response ok200 Nil (RawText "Hello"))

server :: Server IO
server = handle (Proxy :: Proxy Api) impl

config :: Config
config = Config

app :: Wai.Application
app = makeApplication config server

main :: IO ()
main =
  run 8000 app
