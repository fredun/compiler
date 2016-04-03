module Core.TypeLevelSpec where

import Test.Hspec

import Data.Fix (Fix(..))
import qualified Data.Set as Set

import qualified Core.TypeLevel as TypeLevel

spec :: Spec
spec = do

  describe "freeVars" $ do

    it "retrieves none from a type constant" $ do
      let res =
            TypeLevel.freeVars
              ( Fix
                ( TypeLevel.Constant TypeLevel.PrimitiveConstant )
              )
      res `shouldBe` Set.fromList []

    it "retrieves one from a type variable" $ do
      let res =
            TypeLevel.freeVars
              ( Fix
                ( TypeLevel.Variable "foo" )
              )
      res `shouldBe` Set.fromList ["foo"]

    it "retrieves none when shadowed by an abstraction" $ do
      let res =
            TypeLevel.freeVars
              ( Fix
                ( TypeLevel.Abstraction "foo"
                  ( Fix
                    ( TypeLevel.Variable "foo" )
                  )
                )
              )
      res `shouldBe` Set.fromList []

    it "retrieves one when not shadowed by an abstraction" $ do
      let res =
            TypeLevel.freeVars
              ( Fix
                ( TypeLevel.Abstraction "foo"
                  ( Fix
                    ( TypeLevel.Variable "bar" )
                  )
                )
              )
      res `shouldBe` Set.fromList ["bar"]

    it "retrieves two from an application" $ do
      let res =
            TypeLevel.freeVars
              ( Fix
                ( TypeLevel.Application
                  ( Fix
                    ( TypeLevel.Variable "foo" )
                  )
                  ( Fix
                    ( TypeLevel.Variable "bar" )
                  )
                )
              )
      res `shouldBe` Set.fromList ["foo", "bar"]
