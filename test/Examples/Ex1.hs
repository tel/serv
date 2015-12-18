{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Examples.Ex1 where

import           Data.Function       ((&))
import           Data.Proxy
import           Data.String
import           Data.Text           (Text)
import qualified Network.Wai         as Wai
import qualified Network.Wai.Test    as T
import qualified Serv.Api            as A
import           Serv.Common
import qualified Serv.ContentType    as Ct
import qualified Serv.Header         as H
import qualified Serv.Header.Proxies as Hp
import           Serv.Server
import           Test.Tasty
import qualified Test.Tasty.HUnit    as Hu

type RawBody = 'A.Body '[ Ct.TextPlain ] Text
type JSONBody = 'A.Body '[ Ct.JSON ] [Int]

type Api
  = 'A.Endpoint ()
    '[ 'A.Method 'A.GET '[ 'H.CacheControl 'A.::: RawText ] RawBody
     , 'A.Method 'A.PUT '[ 'H.CacheControl 'A.::: RawText ] JSONBody
     ]

apiProxy :: Proxy Api
apiProxy = Proxy

impl :: Impl Api IO
impl = get :<|> put :<|> noOp
  where
    get =
      return
      $ emptyResponse ok200
      & withHeader Hp.cacheControl "foo"
      & withBody "Hello"
    put =
      return
      $ emptyResponse ok200
      & withHeader Hp.cacheControl "foo"
      & withBody [1, 2, 3]


server :: Server IO
server = handle apiProxy impl

runTest :: T.Session a -> IO a
runTest = flip T.runSession (makeApplication defaultConfig server)

test1 :: TestTree
test1 = testGroup "Simple responses"
  [ Hu.testCase "Constant GET response (RawText)" $ runTest $ do
      let req = Wai.defaultRequest
      resp <- T.request req
      T.assertStatus 200 resp
      T.assertContentType "text/plain" resp
      T.assertBody "Hello" resp
      T.assertHeader "Cache-Control" "foo" resp

  , Hu.testCase "404 response (RawText) at ////" $ runTest $ do
      let req = Wai.defaultRequest
                & flip T.setPath "////"
      resp <- T.request req
      T.assertStatus 404 resp

  , Hu.testCase "Constant PUT response (JSON)" $ runTest $ do
      let req = Wai.defaultRequest { Wai.requestMethod = "PUT" }
      resp <- T.request req
      T.assertStatus 200 resp
      T.assertContentType "application/json" resp
      T.assertBody "[1,2,3]" resp
      T.assertHeader "Cache-Control" "foo" resp

  , Hu.testCase "Proper OPTIONS response" $ runTest $ do
      let req = Wai.defaultRequest
                { Wai.requestMethod = "OPTIONS" }
      resp <- T.request req
      T.assertStatus 200 resp
      T.assertBody "" resp
      T.assertHeader "Allow" "GET,HEAD,OPTIONS,PUT" resp

  , Hu.testCase "Proper HEAD response" $ runTest $ do
      let req = Wai.defaultRequest
                { Wai.requestMethod = "HEAD" }
      resp <- T.request req
      T.assertStatus 200 resp
      T.assertBody "" resp
      T.assertHeader "Cache-Control" "foo" resp

  , Hu.testCase "Missing response at bad path" $ runTest $ do
      let req =
            Wai.defaultRequest
            & flip T.setPath "/hello"
      resp <- T.request req
      T.assertStatus 404 resp
      T.assertBody "" resp
      T.assertNoHeader "Cache-Control" resp

  , testGroup "Missing responses at wrong methods"
    $ flip map ["DELETE", "POST"] $ \method ->
      Hu.testCase ("Missing response at method " ++ method) $ runTest $ do
        let req =
              Wai.defaultRequest
              { Wai.requestMethod = fromString method }
        resp <- T.request req
        T.assertStatus 405 resp
        T.assertBody "" resp
        T.assertNoHeader "Cache-Control" resp
  ]

tests :: TestTree
tests = testGroup "Example 1" [ test1 ]
