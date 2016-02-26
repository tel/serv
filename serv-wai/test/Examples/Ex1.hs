{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Examples.Ex1 where

import           Data.String
import           Data.Text        (Text)
import           Serv.Api.Prelude
import           Serv.Wai.Prelude

import qualified Network.Wai      as Wai
import qualified Network.Wai.Test as T
import           Test.Tasty
import qualified Test.Tasty.HUnit as Hu

type RawBody = HasBody '[TextPlain] Text
type JSONBody = HasBody '[JSON] [Int]

type TheApi
  = Endpoint ()
    '[
      Method GET
       '[ Ok :::
          Respond '[ CacheControl ::: Raw Text ] RawBody
        ]
     , Method PUT
       '[ Ok :::
          Respond '[ CacheControl ::: Raw Text ] JSONBody
        ]
     , Method DELETE
       '[ InternalServerError :::
          Respond '[] RawBody
        ]
     ]

apiSing :: Sing TheApi
apiSing = sing

impl :: Impl IO TheApi
impl = get <+> put <+> delete <+> RNil where
  get =
    SGET =:
      (return . respond
         $ emptyResponse SOk
         & withHeader SCacheControl "foo"
         & withBody "Hello")
  put =
    SPUT =:
      (return . respond
         $ emptyResponse SOk
         & withHeader SCacheControl "foo"
         & withBody [1, 2, 3])
  delete =
    SDELETE =:
      (return . respond
         $ emptyResponse SInternalServerError
         & withBody "Server error")

theServer :: Server IO
theServer = server apiSing impl

runTest :: T.Session a -> IO a
runTest = flip T.runSession (serverApplication theServer)

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
      T.assertHeader "Allow" "DELETE,GET,HEAD,OPTIONS,PUT" resp

  , Hu.testCase "Proper HEAD response" $ runTest $ do
      let req = Wai.defaultRequest
                { Wai.requestMethod = "HEAD" }
      resp <- T.request req
      T.assertStatus 200 resp
      T.assertBody "" resp
      T.assertHeader "Cache-Control" "foo" resp

  , Hu.testCase "Error on DELETE response" $ runTest $ do
      let req = Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
      resp <- T.request req
      T.assertStatus 500 resp
      T.assertBody "Server error" resp

  , Hu.testCase "Missing response at bad path" $ runTest $ do
      let req =
            Wai.defaultRequest
            & flip T.setPath "/hello"
      resp <- T.request req
      T.assertStatus 404 resp
      T.assertBody "" resp
      T.assertNoHeader "Cache-Control" resp

  , testGroup "Missing responses at wrong methods"
    $ flip map ["POST", "NOTAMETHOD"] $ \method ->
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
