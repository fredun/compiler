module Core.Unification where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax.Type (TypeF)
import qualified Syntax.Type as Type

import Core.Annotation (Annotated)
import qualified Core.Annotation as Annotation
import Core.Substitution (Substitution)


data UnificationError id =
    ConstantUnificationError Type.Constant Type.Constant
  | OccursUnificationError (Annotated TypeF id) (Annotated TypeF id)
  | GenericUnificationError (Annotated TypeF id) (Annotated TypeF id)
  deriving (Eq, Ord, Show)


mostGeneralUnifier :: Ord id => Annotated TypeF id -> Annotated TypeF id -> Either (UnificationError id) (Substitution TypeF id)
mostGeneralUnifier l@(Fix (Fix.Ann leftAnn left)) r@(Fix (Fix.Ann rightAnn right)) =
  case (left, right) of

    (Type.Variable leftVar, _) ->
      -- Check whether the type variable occurs
      -- in the set of free variables on the right.
      -- If it does, then this is an attempt at
      -- constructing an infinite type and should fail.
      if Set.member leftVar (Annotation.freeVars rightAnn)
        then Left (OccursUnificationError l r)
        else Right (Map.singleton leftVar r)

    (_, Type.Variable rightVar) ->
      -- Check whether the type variable occurs
      -- in the set of free variables on the left.
      -- If it does, then this is an attempt at
      -- constructing an infinite type and should fail.
      if Set.member rightVar (Annotation.freeVars leftAnn)
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
