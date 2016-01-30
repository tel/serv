{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

-- | Here we test that a CORS response is properly handled. We assume that
-- example.com is an acceptable Origin and that authorization IS allowed.
module Examples.Ex2 where

import           Data.Function            ((&))
import           Data.Text                (Text)
import qualified Network.Wai              as Wai
import qualified Network.Wai.Test         as T
import           Serv.Api
import           Serv.Common
import qualified Serv.ContentType         as Ct
import qualified Serv.Cors                as Cors
import qualified Serv.Header              as H
import qualified Serv.Internal.StatusCode as Sc
import           Serv.Server
import           Test.Tasty
import qualified Test.Tasty.HUnit         as Hu

type RawBody = HasBody '[ Ct.TextPlain ] Text

type TheApi
  = Cors Cors.PermitAll :>
    Header H.IfRange (Maybe RawText) :>
    Endpoint ()
      '[ Method GET
         '[ Responding Sc.Ok '[ H.XCsrfToken ::: RawText ] RawBody ]
       , Method DELETE
         '[ Responding Sc.NoContent '[] Empty ]
       ]

apiProxy :: Sing TheApi
apiProxy = sing

impl :: Impl IO TheApi
impl _ifRange = get :<|> delete :<|> MethodNotAllowed
  where
    get =
      respond
      $ emptyResponse Sc.SOk
      & withHeader H.SXCsrfToken (RawText "some-csrf-token")
      & withBody "Hello"
    delete =
      respond
      $ emptyResponse Sc.SNoContent

theServer :: Server IO
theServer = server apiProxy impl

config :: Config
config =
  defaultConfig

runTest :: T.Session a -> IO a
runTest = flip T.runSession (makeApplication config theServer)

test1 :: TestTree
test1 = testGroup "Simple responses"
  [ Hu.testCase "CORS successful GET response" $ runTest $ do
      let req = Wai.defaultRequest
                { Wai.requestHeaders = [("Origin", "http://example.com")] }
      resp <- T.request req
      T.assertStatus 200 resp
      T.assertContentType "text/plain" resp
      T.assertBody "Hello" resp
      T.assertHeader "X-Csrf-Token" "some-csrf-token" resp
      T.assertHeader "Access-Control-Allow-Origin" "http://example.com" resp
      T.assertHeader "Access-Control-Expose-Headers" "X-Csrf-Token" resp
      T.assertHeader "Access-Control-Allow-Credentials" "true" resp
      T.assertHeader "Access-Control-Allow-Headers" "If-Range" resp
      T.assertNoHeader "Access-Control-Allow-Methods" resp

  , Hu.testCase "CORS successful OPTIONS pre-flight response" $ runTest $ do
      let req = Wai.defaultRequest
                { Wai.requestHeaders = [("Origin", "http://example.com")]
                , Wai.requestMethod = "OPTIONS"
                }
      resp <- T.request req
      T.assertStatus 200 resp
      T.assertBody "" resp
      T.assertHeader "Access-Control-Allow-Origin" "http://example.com" resp
      T.assertHeader "Access-Control-Allow-Methods" "DELETE,GET,HEAD,OPTIONS" resp
      T.assertHeader "Access-Control-Expose-Headers" "X-Csrf-Token" resp
      T.assertHeader "Access-Control-Allow-Credentials" "true" resp
      T.assertHeader "Access-Control-Allow-Headers" "If-Range" resp
  ]

tests :: TestTree
tests = testGroup "Example 2 -- CORS" [ test1 ]
