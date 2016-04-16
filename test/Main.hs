module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (consoleTestReporter)
import Test.Tasty.Runners.AntXML

import qualified Core.FreeVarsSpec
import qualified Core.UnificationSpec


main :: IO ()
main = defaultMainWithIngredients [antXMLRunner, consoleTestReporter] tests


tests :: TestTree
tests = testGroup "tests"
  [ testGroup "Core"
    [ Core.FreeVarsSpec.tests
    , Core.UnificationSpec.tests
    ]
  ]
