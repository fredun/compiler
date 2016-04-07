module Core.TypeLevelSpec where

import Test.Hspec

import qualified Data.Set as Set

import qualified Core.TypeLevel as TypeLevel

spec :: Spec
spec = do

  describe "freeVars" $ do

    it "retrieves none from a type constant" $ do
      let res =
            TypeLevel.freeVars
              ( TypeLevel.Type
                ( TypeLevel.Constant TypeLevel.PrimitiveConstant )
              )
      res `shouldBe` Set.fromList []

    it "retrieves one from a type variable" $ do
      let res =
            TypeLevel.freeVars
              ( TypeLevel.Type
                ( TypeLevel.Variable
                  ( TypeLevel.Identifier "foo" )
                )
              )
      res `shouldBe` Set.fromList [TypeLevel.Identifier "foo"]

    it "retrieves none when shadowed by an abstraction" $ do
      let res =
            TypeLevel.freeVars
              ( TypeLevel.Type
                ( TypeLevel.Abstraction
                  ( TypeLevel.Identifier "foo" )
                  ( TypeLevel.Type
                    ( TypeLevel.Variable
                      ( TypeLevel.Identifier "foo" )
                    )
                  )
                )
              )
      res `shouldBe` Set.fromList []

    it "retrieves one when not shadowed by an abstraction" $ do
      let res =
            TypeLevel.freeVars
              ( TypeLevel.Type
                ( TypeLevel.Abstraction
                  ( TypeLevel.Identifier "foo" )
                  ( TypeLevel.Type
                    ( TypeLevel.Variable
                      ( TypeLevel.Identifier "bar" )
                    )
                  )
                )
              )
      res `shouldBe` Set.fromList [TypeLevel.Identifier "bar"]

    it "retrieves two from an application" $ do
      let res =
            TypeLevel.freeVars
              ( TypeLevel.Type
                ( TypeLevel.Application
                  ( TypeLevel.Type
                    ( TypeLevel.Variable
                      ( TypeLevel.Identifier "foo" )
                    )
                  )
                  ( TypeLevel.Type
                    ( TypeLevel.Variable
                      ( TypeLevel.Identifier "bar" )
                    )
                  )
                )
              )
      res `shouldBe` Set.fromList [TypeLevel.Identifier "foo", TypeLevel.Identifier "bar"]
