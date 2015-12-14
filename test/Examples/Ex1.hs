{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Examples.Ex1 where

import           Data.Function       ((&))
import           Data.Proxy
import           Data.Text
import qualified Network.Wai         as Wai
import qualified Network.Wai.Test    as T
import qualified Serv.Api            as A
import qualified Serv.ContentType    as Ct
import qualified Serv.Header         as H
import qualified Serv.Header.Proxies as Hp
import           Serv.Server
import           Serv.Common
import           Test.HUnit
import           Test.Tasty
import qualified Test.Tasty.HUnit    as Hu

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

runTest :: T.Session a -> IO a
runTest = flip T.runSession (makeApplication defaultConfig server)

test1 :: TestTree
test1 = testGroup "Simple responses"
  [ Hu.testCase "Constant GET response" $ runTest $ do
      let req =
            T.defaultRequest
            & flip T.setPath ""
      resp <- T.request req
      T.assertStatus 200 resp
      T.assertContentType "text/plain" resp
      T.assertBody "Hello" resp
      T.assertHeader "Cache-Control" "foo" resp
  ]

tests :: TestTree
tests = testGroup "Example 1" [ test1 ]
