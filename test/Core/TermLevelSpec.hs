module Core.TermLevelSpec where

import Test.Hspec

import Data.Fix (Fix(..))
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Core.TypeLevel as TypeLevel
import qualified Core.TermLevel as TermLevel

spec :: Spec
spec = do

  describe "freeVars" $ do

    it "retrieves none from a constant" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.Constant )
              )
      res `shouldBe` Set.fromList []

    it "retrieves one from a variable" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.Variable
                  ( TermLevel.Identifier "foo" )
                )
              )
      res `shouldBe` Set.fromList [TermLevel.Identifier "foo"]

    it "retrieves none when shadowed by an abstraction" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.Abstraction
                  ( TermLevel.Identifier "foo" )
                  ( Fix
                    ( TermLevel.Variable
                      ( TermLevel.Identifier "foo" )
                    )
                  )
                )
              )
      res `shouldBe` Set.fromList []

    it "retrieves one when not shadowed by an abstraction" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.Abstraction
                  ( TermLevel.Identifier "foo" )
                  ( Fix
                    ( TermLevel.Variable
                      ( TermLevel.Identifier "bar" )
                    )
                  )
                )
              )
      res `shouldBe` Set.fromList [TermLevel.Identifier "bar"]

    it "retrieves two from an application" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.Application
                  ( Fix
                    ( TermLevel.Variable
                      ( TermLevel.Identifier "foo" )
                    )
                  )
                  ( Fix
                    ( TermLevel.Variable
                      ( TermLevel.Identifier "bar" )
                    )
                  )
                )
              )
      res `shouldBe` Set.fromList [TermLevel.Identifier "foo", TermLevel.Identifier "bar"]

    it "retrieves one from a type abstraction" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.TypeAbstraction
                  ( TypeLevel.Identifier "foo" )
                  ( Fix
                    ( TermLevel.Variable
                      ( TermLevel.Identifier "bar" )
                    )
                  )
                )
              )
      res `shouldBe` Set.fromList [TermLevel.Identifier "bar"]

    it "retrieves one from a type application" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.TypeApplication
                  ( Fix
                    ( TermLevel.Variable
                      ( TermLevel.Identifier "bar" )
                    )
                  )
                  ( Fix
                    ( TypeLevel.Variable
                      ( TypeLevel.Identifier "foo" )
                    )
                  )
                )
              )
      res `shouldBe` Set.fromList [TermLevel.Identifier "bar"]

    it "retrieves many from a record introduction" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.RecordIntroduction
                  ( Map.fromList
                    [ ( TermLevel.Identifier "1"
                      , Fix
                        ( TermLevel.Variable
                          ( TermLevel.Identifier "bar" )
                        )
                      )
                    , ( TermLevel.Identifier "2"
                      , Fix
                        ( TermLevel.Variable
                          ( TermLevel.Identifier "foo" )
                        )
                      )
                    ]
                  )
                )
              )
      res `shouldBe` Set.fromList [TermLevel.Identifier "foo", TermLevel.Identifier "bar"]

    it "retrieves one from a record elimination" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.RecordElimination
                  ( Fix
                    ( TermLevel.Variable
                      ( TermLevel.Identifier "bar" )
                    )
                  )
                  ( TermLevel.Identifier "foo" )
                )
              )
      res `shouldBe` Set.fromList [TermLevel.Identifier "bar"]
