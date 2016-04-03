module Main where

import Test.Hspec

import qualified ParserSpec
import qualified Core.TypeLevelSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" ParserSpec.spec

  describe "Core" $ do

    describe "TypeLevel" Core.TypeLevelSpec.spec
