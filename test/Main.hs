module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (consoleTestReporter)
import Test.Tasty.Runners.AntXML

import qualified ParserSpec
import qualified Core.TypeLevelSpec
import qualified Core.TermLevelSpec


main :: IO ()
main = defaultMainWithIngredients [antXMLRunner, consoleTestReporter] tests


tests :: TestTree
tests = testGroup "tests"
  [ ParserSpec.tests
  , testGroup "Core"
    [ Core.TypeLevelSpec.tests
    , Core.TermLevelSpec.tests
    ]
  ]
