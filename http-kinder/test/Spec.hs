
import           Test.Tasty
import qualified Test.Tasty.HUnit             as Hu
import           Test.Tasty.Ingredients.Basic (consoleTestReporter,
                                               listingTests)
import           Test.Tasty.Runners.AntXML    (antXMLRunner)
import           Test.HUnit
import qualified Test.Network.HTTP.Kinder.URI as URI

main :: IO ()
main =
  defaultMainWithIngredients
  [ antXMLRunner
  , listingTests
  , consoleTestReporter
  ] tests

tests :: TestTree
tests =
  testGroup "Server Tests"
  [ systemTests
  , URI.tests
  ]

systemTests :: TestTree
systemTests = testGroup "System Tests"
  [ Hu.testCase "Trivial tests" trivial
  ] where

trivial :: Assertion
trivial = assertBool "True is False" True
