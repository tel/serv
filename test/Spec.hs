
import           Test.Tasty
import qualified Test.Tasty.HUnit             as Hu
import           Test.Tasty.Ingredients.Basic (consoleTestReporter,
                                               listingTests)
import           Test.Tasty.Runners.AntXML    (antXMLRunner)
import           Test.HUnit
import qualified Examples.Ex1 as Ex1

main :: IO ()
main =
  defaultMainWithIngredients
  [ antXMLRunner
  , listingTests
  , consoleTestReporter
  ] tests

tests :: TestTree
tests =
  testGroup "Tests"
  [ systemTests
  , Ex1.tests
  ]

systemTests :: TestTree
systemTests = testGroup "System Tests"
  [ Hu.testCase "Trivial tests" trivial
  ] where

trivial :: Assertion
trivial = assertBool "True is False" True
