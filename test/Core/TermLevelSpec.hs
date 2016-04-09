{-# LANGUAGE QuasiQuotes #-}

module Core.TermLevelSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Core.TypeLevel as TypeLevel
import qualified Core.TermLevel as TermLevel

import IR.DSL

tests :: TestTree
tests = testGroup "TermLevel"

  [ testGroup "freeVars"

    [ testCase "retrieves none from a constant" $ do
        let res = TermLevel.freeVars [termDSL|
          (constant (numeric (integer 32 -1)))
        |]
        res @?= Set.fromList []

    , testCase "retrieves one from a variable" $ do
        let res = TermLevel.freeVars [termDSL|
          (variable "foo")
        |]
        res @?= Set.fromList [TermLevel.Identifier "foo"]

    , testCase "retrieves none when shadowed by an abstraction" $ do
        let res = TermLevel.freeVars [termDSL|
          (abstraction "foo" (variable "foo"))
        |]
        res @?= Set.fromList []

    , testCase "retrieves one when not shadowed by an abstraction" $ do
        let res = TermLevel.freeVars [termDSL|
          (abstraction "foo" (variable "bar"))
        |]
        res @?= Set.fromList [TermLevel.Identifier "bar"]

    , testCase "retrieves two from an application" $ do
        let res = TermLevel.freeVars [termDSL|
          (application (variable "foo") (variable "bar"))
        |]
        res @?= Set.fromList [TermLevel.Identifier "foo", TermLevel.Identifier "bar"]

    , testCase "retrieves one from a type abstraction" $ do
        let res = TermLevel.freeVars [termDSL|
          (type-abstraction "foo" (variable "bar"))
        |]
        res @?= Set.fromList [TermLevel.Identifier "bar"]

    , testCase "retrieves one from a type application" $ do
        let res = TermLevel.freeVars [termDSL|
          (type-application (variable "bar") (type-variable "foo"))
        |]
        res @?= Set.fromList [TermLevel.Identifier "bar"]

    , testCase "retrieves many from a record introduction" $ do
        let res =
              TermLevel.freeVars
                ( TermLevel.Term
                  ( Fix
                    ( TermLevel.RecordIntroduction
                      ( Map.fromList
                        [ ( "1"
                          , Fix
                            ( TermLevel.Variable
                              ( TermLevel.Identifier "bar" )
                            )
                          )
                        , ( "2"
                          , Fix
                            ( TermLevel.Variable
                              ( TermLevel.Identifier "foo" )
                            )
                          )
                        ]
                      )
                    )
                  )
                )
        res @?= Set.fromList [TermLevel.Identifier "foo", TermLevel.Identifier "bar"]

    , testCase "retrieves one from a record elimination" $ do
        let res =
              TermLevel.freeVars
                ( TermLevel.Term
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
                )
        res @?= Set.fromList [TermLevel.Identifier "bar"]

    ]

  ]
