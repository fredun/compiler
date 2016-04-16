module Core.Unification where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Syntax.Type as Type

import Core.Annotation (AnnotatedType)
import qualified Core.Annotation as Annotation
import Core.Substitution (TypeSubstitution)


data UnificationError =
    ConstantUnificationError Type.Constant Type.Constant
  | OccursUnificationError AnnotatedType AnnotatedType
  | GenericUnificationError AnnotatedType AnnotatedType
  deriving (Eq, Ord, Show)


mostGeneralUnifier :: AnnotatedType -> AnnotatedType -> Either UnificationError TypeSubstitution
mostGeneralUnifier l@(Fix (Fix.Ann leftAnn left)) r@(Fix (Fix.Ann rightAnn right)) =
  case (left, right) of

    (Type.Variable leftVar, _) ->
      -- Check whether the type variable occurs
      -- in the set of free variables on the right.
      -- If it does, then this is an attempt at
      -- constructing an infinite type and should fail.
      if Set.member leftVar (Annotation.freeTypeVars rightAnn)
        then Left (OccursUnificationError l r)
        else Right (Map.singleton leftVar r)

    (_, Type.Variable rightVar) ->
      -- Check whether the type variable occurs
      -- in the set of free variables on the left.
      -- If it does, then this is an attempt at
      -- constructing an infinite type and should fail.
      if Set.member rightVar (Annotation.freeTypeVars leftAnn)
        then Left (OccursUnificationError l r)
        else Right (Map.singleton rightVar l)

    (Type.Constant leftConst, Type.Constant rightConst) ->
      -- Check whether the constants on both sides are equal.
      -- If they're not, it is a unification error.
      if leftConst == rightConst
        then Right Map.empty
        else Left (ConstantUnificationError leftConst rightConst)

    _ ->
      Left (GenericUnificationError l r)
