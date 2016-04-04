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
                ( TermLevel.Variable "foo" )
              )
      res `shouldBe` Set.fromList ["foo"]

    it "retrieves none when shadowed by an abstraction" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.Abstraction "foo"
                  ( Fix
                    ( TermLevel.Variable "foo" )
                  )
                )
              )
      res `shouldBe` Set.fromList []

    it "retrieves one when not shadowed by an abstraction" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.Abstraction "foo"
                  ( Fix
                    ( TermLevel.Variable "bar" )
                  )
                )
              )
      res `shouldBe` Set.fromList ["bar"]

    it "retrieves two from an application" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.Application
                  ( Fix
                    ( TermLevel.Variable "foo" )
                  )
                  ( Fix
                    ( TermLevel.Variable "bar" )
                  )
                )
              )
      res `shouldBe` Set.fromList ["foo", "bar"]

    it "retrieves one from a type abstraction" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.TypeAbstraction "foo"
                  ( Fix
                    ( TermLevel.Variable "bar" )
                  )
                )
              )
      res `shouldBe` Set.fromList ["bar"]

    it "retrieves one from a type application" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.TypeApplication
                  ( Fix
                    ( TermLevel.Variable "bar" )
                  )
                  ( Fix
                    ( TypeLevel.Variable "foo" )
                  )
                )
              )
      res `shouldBe` Set.fromList ["bar"]

    it "retrieves many from a record introduction" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.RecordIntroduction
                  ( Map.fromList
                    [ ( "1", Fix ( TermLevel.Variable "bar" ) )
                    , ( "2", Fix ( TermLevel.Variable "foo" ) )
                    ]
                  )
                )
              )
      res `shouldBe` Set.fromList ["foo", "bar"]

    it "retrieves one from a record elimination" $ do
      let res =
            TermLevel.freeVars
              ( Fix
                ( TermLevel.RecordElimination
                  ( Fix
                    ( TermLevel.Variable "bar" )
                  )
                  "foo"
                )
              )
      res `shouldBe` Set.fromList ["bar"]
