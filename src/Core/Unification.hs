module Core.Unification where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Type (Type, TypeF)
import qualified Syntax.Type as Type


type AnnotatedType = Fix.Attr TypeF (Set Type.Identifier)


data UnificationError =
    ConstantUnificationError Type.Constant Type.Constant
  | OccursUnificationError AnnotatedType AnnotatedType
  | GenericUnificationError AnnotatedType AnnotatedType
  deriving (Eq, Ord, Show)


mostGeneralUnifier :: AnnotatedType -> AnnotatedType -> Either UnificationError (Map Type.Identifier AnnotatedType)
mostGeneralUnifier l@(Fix (Fix.Ann leftFree left)) r@(Fix (Fix.Ann rightFree right)) =
  case (left, right) of

    (Type.Variable leftVar, _) ->
      -- Check whether the type variable occurs
      -- in the set of free variables on the right.
      -- If it does, then this is an attempt at
      -- constructing an infinite type and should fail.
      if Set.member leftVar rightFree
        then Left (OccursUnificationError l r)
        else Right (Map.singleton leftVar r)

    (_, Type.Variable rightVar) ->
      -- Check whether the type variable occurs
      -- in the set of free variables on the left.
      -- If it does, then this is an attempt at
      -- constructing an infinite type and should fail.
      if Set.member rightVar leftFree
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


substitute :: Type -> Map Type.Identifier Type -> Type
substitute (Fix typeF) subs =
  case typeF of

    Type.Constant c ->
      Fix (Type.Constant c)

    Type.Variable var ->
      case Map.lookup var subs of
        Just sub ->
          substitute sub subs
        Nothing ->
          Fix typeF

    Type.Abstraction arg kind body ->
      let
        bodySubs = Map.delete arg subs
      in
        Fix (Type.Abstraction arg kind (substitute body bodySubs))

    Type.Application body arg ->
      Fix (Type.Application (substitute body subs) (substitute arg subs))
