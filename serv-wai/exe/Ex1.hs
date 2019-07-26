{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Aeson (Value (..))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Network.Wai.Handler.Warp (run)

import           Serv.Api.Prelude
import           Serv.Wai.Prelude

type RawBody = HasBody '[TextPlain] Text
type JSONBody = HasBody '[JSON] Value

type TheApi
  = Endpoint ()
    '[ GET ::: Outputs
       '[ Ok :::
          Respond '[ CacheControl ::: Raw Text ] RawBody
        ]
     , PUT ::: CaptureBody '[JSON] (Maybe Value) (Outputs
         '[ Ok :::
            Respond '[ CacheControl ::: Raw Text ] JSONBody
          ])
     , DELETE ::: Outputs
       '[ InternalServerError :::
          Respond '[] RawBody
        ]
     ]

apiSing :: Sing TheApi
apiSing = sing

impl :: Impl IO TheApi
impl = get <+> put <+> delete where
  get =
    SGET =:
      (return . respond
         $ emptyResponse SOk
         & withHeader SCacheControl "foo"
         & withBody "Hello")
  put =
    SPUT =:
      (\body -> return . respond
         $ emptyResponse SOk
         & withHeader SCacheControl "foo"
         & withBody (fromMaybe (String "no body passed") body))
  delete =
    SDELETE =:
      (return . respond
         $ emptyResponse SInternalServerError
         & withBody "Server error")

theServer :: Server IO
theServer = server apiSing impl

main :: IO ()
main = run 3000 (serverApplication theServer)
