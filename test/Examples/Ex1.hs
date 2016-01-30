{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Examples.Ex1 where

import           Data.Function            ((&))
import           Data.String
import           Data.Text                (Text)
import qualified Network.Wai              as Wai
import qualified Network.Wai.Test         as T
import           Serv.Api
import           Serv.Common
import qualified Serv.ContentType         as Ct
import qualified Serv.Header              as H
import qualified Serv.Internal.StatusCode as Sc
import           Serv.Server
import           Test.Tasty
import qualified Test.Tasty.HUnit         as Hu

type RawBody = HasBody '[ Ct.TextPlain ] Text
type JSONBody = HasBody '[ Ct.JSON ] [Int]

type TheApi
  = Endpoint ()
    '[ Method GET
       '[ Responding Sc.Ok
          '[ 'H.CacheControl ::: RawText ] RawBody
        ]
     , Method PUT
       '[ Responding Sc.Ok
          '[ 'H.CacheControl ::: RawText ] JSONBody
        ]
     , Method DELETE
       '[ Responding Sc.InternalServerError
          '[] RawBody
        ]
     ]

apiSing :: Sing TheApi
apiSing = sing

impl :: Impl IO TheApi
impl = get :<|> put :<|> delete :<|> MethodNotAllowed
  where
    get =
      respond
      $ emptyResponse Sc.SOk
      & withHeader H.SCacheControl "foo"
      & withBody "Hello"
    put =
      respond
      $ emptyResponse Sc.SOk
      & withHeader H.SCacheControl "foo"
      & withBody [1, 2, 3]
    delete =
      respond
      $ emptyResponse Sc.SInternalServerError
      & withBody "Server error"


theServer :: Server IO
theServer = server apiSing impl

runTest :: T.Session a -> IO a
runTest = flip T.runSession (makeApplication defaultConfig theServer)

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
