{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

import           Data.Function            ((&))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Serv.Api
import           Serv.Common
import qualified Serv.ContentType         as Ct
import qualified Serv.Cors                as Cors
import qualified Serv.Header              as H
import           Serv.Server

type RawBody = HasBody '[ Ct.TextPlain ] Text

type TheApi
  = Cors Cors.PermitAll :>
    Endpoint ()
      '[ Method GET '[ H.CacheControl ::: RawText ] RawBody ]


apiSing :: Sing TheApi
apiSing = sing

impl :: Impl IO TheApi
impl = get :<|> MethodNotAllowed
  where
    get =
      return
      $ emptyResponse ok200
      & withHeader H.SCacheControl "foo"
      & withBody "Hello"

theServer :: Server IO
theServer = server apiSing impl

main :: IO ()
main =
  run 8000 (makeApplication defaultConfig theServer)
