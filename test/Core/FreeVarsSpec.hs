{-# LANGUAGE QuasiQuotes #-}

module Core.FreeVarsSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Syntax.Type as Type
import qualified Syntax.Term as Term
import qualified Core.FreeVars as FreeVars

import IR.DSL

tests :: TestTree
tests = testGroup "FreeVaes"

  [ testGroup "freeVars"

    [ testCase "retrieves none from a constant" $ do
        let res = FreeVars.freeVars ([termDSL|
          (constant (numeric (integer 32 -1)))
        |] :: Term.Term Type.Identifier Term.Identifier)
        res @?= Set.fromList []

    , testCase "retrieves one from a variable" $ do
        let res = FreeVars.freeVars ([termDSL|
          (variable "foo")
        |] :: Term.Term Type.Identifier Term.Identifier)
        res @?= Set.fromList [Term.Identifier "foo"]

    , testCase "retrieves none when shadowed by an abstraction" $ do
        let res = FreeVars.freeVars ([termDSL|
          (abstraction "foo" (variable "foo"))
        |] :: Term.Term Type.Identifier Term.Identifier)
        res @?= Set.fromList []

    , testCase "retrieves one when not shadowed by an abstraction" $ do
        let res = FreeVars.freeVars ([termDSL|
          (abstraction "foo" (variable "bar"))
        |] :: Term.Term Type.Identifier Term.Identifier)
        res @?= Set.fromList [Term.Identifier "bar"]

    , testCase "retrieves two from an application" $ do
        let res = FreeVars.freeVars ([termDSL|
          (application (variable "foo") (variable "bar"))
        |] :: Term.Term Type.Identifier Term.Identifier)
        res @?= Set.fromList [Term.Identifier "foo", Term.Identifier "bar"]

    , testCase "retrieves one from a type abstraction" $ do
        let res = FreeVars.freeVars ([termDSL|
          (type-abstraction "foo" (type) (variable "bar"))
        |] :: Term.Term Type.Identifier Term.Identifier)
        res @?= Set.fromList [Term.Identifier "bar"]

    , testCase "retrieves one from a type application" $ do
        let res = FreeVars.freeVars ([termDSL|
          (type-application (variable "bar") (type-variable "foo"))
        |] :: Term.Term Type.Identifier Term.Identifier)
        res @?= Set.fromList [Term.Identifier "bar"]

    , testCase "retrieves many from a record introduction" $ do
        let res =
              FreeVars.freeVars
                ( Fix
                  ( Term.RecordIntroduction
                    ( Map.fromList
                      [ ( "1"
                        , Fix
                          ( Term.Variable
                            ( Term.Identifier "bar" )
                          )
                        )
                      , ( "2"
                        , Fix
                          ( Term.Variable
                            ( Term.Identifier "foo" )
                          )
                        )
                      ]
                    )
                  )
                )
        res @?= Set.fromList [Term.Identifier "foo", Term.Identifier "bar"]

    , testCase "retrieves one from a record elimination" $ do
        let res =
              FreeVars.freeVars
                ( Fix
                  ( Term.RecordElimination
                    ( Fix
                      ( Term.Variable
                        ( Term.Identifier "bar" )
                      )
                    )
                    "foo"
                  )
                )
        res @?= Set.fromList [Term.Identifier "bar"]

    ]

  , testGroup "freeVarsType"

    [ testCase "retrieves none from a type constant" $ do
        let res =
              FreeVars.freeVarsType
                ( Fix
                  ( Type.Constant
                    ( Type.PrimitiveConstant Type.StringPrimitive )
                  )
                  :: Type.Type Type.Identifier
                )
        res @?= Set.fromList []

    , testCase "retrieves one from a type variable" $ do
        let res =
              FreeVars.freeVarsType
                ( Fix
                  ( Type.Variable
                    ( Type.Identifier "foo" )
                  )
                  :: Type.Type Type.Identifier
                )
        res @?= Set.fromList [Type.Identifier "foo"]

    , testCase "retrieves none when shadowed by an abstraction" $ do
        let res =
              FreeVars.freeVarsType
                ( Fix
                  ( Type.Abstraction
                    ( Type.Identifier "foo" )
                    Type.KindOfTypes
                    ( Fix
                      ( Type.Variable
                        ( Type.Identifier "foo" )
                      )
                    )
                  )
                  :: Type.Type Type.Identifier
                )
        res @?= Set.fromList []

    , testCase "retrieves one when not shadowed by an abstraction" $ do
        let res =
              FreeVars.freeVarsType
                ( Fix
                  ( Type.Abstraction
                    ( Type.Identifier "foo" )
                    Type.KindOfTypes
                    ( Fix
                      ( Type.Variable
                        ( Type.Identifier "bar" )
                      )
                    )
                  )
                  :: Type.Type Type.Identifier
                )
        res @?= Set.fromList [Type.Identifier "bar"]

    , testCase "retrieves two from an application" $ do
        let res =
              FreeVars.freeVarsType
                ( Fix
                  ( Type.Application
                    ( Fix
                      ( Type.Variable
                        ( Type.Identifier "foo" )
                      )
                    )
                    ( Fix
                      ( Type.Variable
                        ( Type.Identifier "bar" )
                      )
                    )
                  )
                  :: Type.Type Type.Identifier
                )
        res @?= Set.fromList [Type.Identifier "foo", Type.Identifier "bar"]

    ]

  ]
