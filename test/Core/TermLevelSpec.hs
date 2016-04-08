{-# LANGUAGE QuasiQuotes #-}

module Core.TermLevelSpec where

import Test.Hspec

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Core.TypeLevel as TypeLevel
import qualified Core.TermLevel as TermLevel

import Core.DSL

spec :: Spec
spec = do

  describe "freeVars" $ do

    it "retrieves none from a constant" $ do
      let res = TermLevel.freeVars [dsl|
        (constant (numeric 42))
      |]
      res `shouldBe` Set.fromList []

    it "retrieves one from a variable" $ do
      let res = TermLevel.freeVars [dsl|
        (variable "foo")
      |]
      res `shouldBe` Set.fromList [TermLevel.Identifier "foo"]

    it "retrieves none when shadowed by an abstraction" $ do
      let res = TermLevel.freeVars [dsl|
        (abstraction "foo" (variable "foo"))
      |]
      res `shouldBe` Set.fromList []

    it "retrieves one when not shadowed by an abstraction" $ do
      let res = TermLevel.freeVars [dsl|
        (abstraction "foo" (variable "bar"))
      |]
      res `shouldBe` Set.fromList [TermLevel.Identifier "bar"]

    it "retrieves two from an application" $ do
      let res = TermLevel.freeVars [dsl|
        (application (variable "foo") (variable "bar"))
      |]
      res `shouldBe` Set.fromList [TermLevel.Identifier "foo", TermLevel.Identifier "bar"]

    it "retrieves one from a type abstraction" $ do
      let res = TermLevel.freeVars [dsl|
        (type-abstraction "foo" (variable "bar"))
      |]
      res `shouldBe` Set.fromList [TermLevel.Identifier "bar"]

    it "retrieves one from a type application" $ do
      let res = TermLevel.freeVars [dsl|
        (type-application (variable "bar") (variable "foo"))
      |]
      res `shouldBe` Set.fromList [TermLevel.Identifier "bar"]

    it "retrieves many from a record introduction" $ do
      let res =
            TermLevel.freeVars
              ( TermLevel.Term
                ( TermLevel.RecordIntroduction
                  ( Map.fromList
                    [ ( "1"
                      , TermLevel.Term
                        ( TermLevel.Variable
                          ( TermLevel.Identifier "bar" )
                        )
                      )
                    , ( "2"
                      , TermLevel.Term
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
              ( TermLevel.Term
                ( TermLevel.RecordElimination
                  ( TermLevel.Term
                    ( TermLevel.Variable
                      ( TermLevel.Identifier "bar" )
                    )
                  )
                  ( TermLevel.Identifier "foo" )
                )
              )
      res `shouldBe` Set.fromList [TermLevel.Identifier "bar"]
