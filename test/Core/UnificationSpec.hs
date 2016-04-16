module Core.UnificationSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Syntax.Type as Type
import qualified Syntax.Term as Term

import qualified Core.FreeVars as FreeVars
import qualified Core.Unification as Unification
import Core.Annotation (TypeAnnotation(..))

tests :: TestTree
tests = testGroup "Unification"

  [ testGroup "mostGeneralUnifier"

    [ testCase "fails when the occurs check fails on the left" $ do
        let left = Fix (Fix.Ann (TypeAnnotation (Set.singleton (Type.Identifier "a"))) (Type.Constant Type.FunctionConstant))
        let right = Fix (Fix.Ann (TypeAnnotation Set.empty) (Type.Variable (Type.Identifier "a")))
        Unification.mostGeneralUnifier left right @?= Left (Unification.OccursUnificationError left right)

    , testCase "fails when the occurs check fails on the right" $ do
        let left = Fix (Fix.Ann (TypeAnnotation Set.empty) (Type.Variable (Type.Identifier "a")))
        let right = Fix (Fix.Ann (TypeAnnotation (Set.singleton (Type.Identifier "a"))) (Type.Constant Type.FunctionConstant))
        Unification.mostGeneralUnifier left right @?= Left (Unification.OccursUnificationError left right)

    , testCase "succeeds with a type variable on the left" $ do
        let left = Fix (Fix.Ann (TypeAnnotation (Set.singleton (Type.Identifier "a"))) (Type.Variable (Type.Identifier "a")))
        let right = Fix (Fix.Ann (TypeAnnotation Set.empty) (Type.Constant Type.FunctionConstant))
        Unification.mostGeneralUnifier left right @?= Right (Map.singleton (Type.Identifier "a") right)

    , testCase "succeeds with a type variable on the right" $ do
        let left = Fix (Fix.Ann (TypeAnnotation Set.empty) (Type.Constant Type.FunctionConstant))
        let right = Fix (Fix.Ann (TypeAnnotation (Set.singleton (Type.Identifier "a"))) (Type.Variable (Type.Identifier "a")))
        Unification.mostGeneralUnifier left right @?= Right (Map.singleton (Type.Identifier "a") left)

    ]

  ]
