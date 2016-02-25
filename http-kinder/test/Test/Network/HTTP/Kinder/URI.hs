{-# LANGUAGE OverloadedStrings #-}

module Test.Network.HTTP.Kinder.URI where

import           Data.Proxy
import qualified Data.Text               as Text
import           Network.HTTP.Kinder.URI
import           Test.HUnit
import           Test.Tasty
import qualified Test.Tasty.HUnit        as Hu

tests :: TestTree
tests =
  testGroup "URI Serialization"
  [ testGroup "Decoding"
    [ testGroup "Int" $ testDecodeDecimal (Proxy :: Proxy Int)
    , testGroup "Integer" $ testDecodeDecimal (Proxy :: Proxy Integer)
    ]
  ]

testDecodeDecimal :: (URIDecode a, Num a, Show a, Eq a) => Proxy a -> [TestTree]
testDecodeDecimal p =
  [ Hu.testCase "decode simple correct examples" $ do
      testParse "3" 3
      testParse "3000" 3000
      testParse "9999" 9999
      testParse "0000" 0
  , Hu.testCase "fail to decode simple incorrect examples" $ do
      testNoParse "123abc"
      testNoParse "abc123"
      testNoParse ""
      testNoParse "foobar"
  ]

  where
    testParse text val =
      assertEqual
        ("Value '" ++ text ++ "' parses")
        (Right (val `asProxyTypeOf` p))
        (uriDecode (Text.pack text))
    testNoParse text =
      case uriDecode (Text.pack text) of
        Left _err -> assert True
        Right v ->
          assertFailure
            ("successfully parsed bad value: " ++ text ++ " as " ++ show (v `asProxyTypeOf` p))

