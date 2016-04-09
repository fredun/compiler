module Core.TypeLevelSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Set as Set

import qualified Core.TypeLevel as TypeLevel

tests :: TestTree
tests = testGroup "TypeLevel"

  [ testGroup "freeVars"

    [ testCase "retrieves none from a type constant" $ do
        let res =
              TypeLevel.freeVars
                ( TypeLevel.Type
                  ( Fix
                    ( TypeLevel.Constant TypeLevel.PrimitiveConstant )
                  )
                )
        res @?= Set.fromList []

    , testCase "retrieves one from a type variable" $ do
        let res =
              TypeLevel.freeVars
                ( TypeLevel.Type
                  ( Fix
                    ( TypeLevel.Variable
                      ( TypeLevel.Identifier "foo" )
                    )
                  )
                )
        res @?= Set.fromList [TypeLevel.Identifier "foo"]

    , testCase "retrieves none when shadowed by an abstraction" $ do
        let res =
              TypeLevel.freeVars
                ( TypeLevel.Type
                  ( Fix
                    ( TypeLevel.Abstraction
                      ( TypeLevel.Identifier "foo" )
                      ( Fix
                        ( TypeLevel.Variable
                          ( TypeLevel.Identifier "foo" )
                        )
                      )
                    )
                  )
                )
        res @?= Set.fromList []

    , testCase "retrieves one when not shadowed by an abstraction" $ do
        let res =
              TypeLevel.freeVars
                ( TypeLevel.Type
                  ( Fix
                    ( TypeLevel.Abstraction
                      ( TypeLevel.Identifier "foo" )
                      ( Fix
                        ( TypeLevel.Variable
                          ( TypeLevel.Identifier "bar" )
                        )
                      )
                    )
                  )
                )
        res @?= Set.fromList [TypeLevel.Identifier "bar"]

    , testCase "retrieves two from an application" $ do
        let res =
              TypeLevel.freeVars
                ( TypeLevel.Type
                  ( Fix
                    ( TypeLevel.Application
                      ( Fix
                        ( TypeLevel.Variable
                          ( TypeLevel.Identifier "foo" )
                        )
                      )
                      ( Fix
                        ( TypeLevel.Variable
                          ( TypeLevel.Identifier "bar" )
                        )
                      )
                    )
                  )
                )
        res @?= Set.fromList [TypeLevel.Identifier "foo", TypeLevel.Identifier "bar"]

    ]

  ]
