module Main where

import Test.Hspec

import qualified ParserSpec
import qualified Core.TypeLevelSpec
import qualified Core.TermLevelSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" ParserSpec.spec

  describe "Core" $ do

    describe "TypeLevel" Core.TypeLevelSpec.spec
    describe "TermLevel" Core.TermLevelSpec.spec
