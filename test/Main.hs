module Main where

import Test.Hspec

import qualified ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" ParserSpec.spec
