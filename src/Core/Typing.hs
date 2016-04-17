module Core.Typing where

import Control.Monad (forM)

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Type (Type)
import qualified Syntax.Type as Type
import Syntax.Term (TermF)
import qualified Syntax.Term as Term

import Core.Unique (UniqueId(..), UniqueT)
import qualified Core.Unique as Unique


data Constraint typeId =
    EqualityConstraint (Type typeId) (Type typeId)
  deriving (Eq, Ord, Show)


data Result typeId id = Result
  { constraints :: Set (Constraint typeId)
  , assumptions :: Map id (Set (Type typeId))
  }
  deriving (Eq, Ord, Show)


instance (Ord typeId, Ord id) => Monoid (Result typeId id) where
  mempty =
    Result
      { constraints = Set.empty
      , assumptions = Map.empty
      }
  mappend a b =
    Result
      { constraints = Set.union (constraints a) (constraints b)
      , assumptions = Map.unionWith Set.union (assumptions a) (assumptions b)
      }


type TermId = UniqueId Term.Identifier
type TypeId = UniqueId Type.Identifier


generateConstraintsForConstant :: Monad m =>
  Term.Constant ->
  UniqueT m (Type TypeId, Result TypeId TermId)
generateConstraintsForConstant constant =
  pure (Fix (Type.Constant (Type.PrimitiveConstant (typeOfConstant constant))), mempty)


generateConstraintsForVariable :: Monad m =>
  UniqueId Term.Identifier ->
  UniqueT m (Type TypeId, Result TypeId TermId)
generateConstraintsForVariable uniqueId@(UniqueId _ (Term.Identifier var)) =
  do
    typeId <- Unique.unique (Type.Identifier var)
    let
      typeVar =
        Fix (Type.Variable typeId)
      res =
        Result mempty (Map.singleton uniqueId (Set.singleton typeVar))
    pure (typeVar, res)


generateConstraintsForAbstraction :: Monad m =>
  [UniqueId Term.Identifier] ->
  (Type TypeId, Result TypeId TermId) ->
  UniqueT m (Type TypeId, Result TypeId TermId)
generateConstraintsForAbstraction vars body =
  do
    typeIds <- forM vars $ \(UniqueId _ (Term.Identifier var)) ->
      Unique.unique (Type.Identifier var)
    let
      typeVars =
        map (\typeId -> Fix (Type.Variable typeId)) typeIds
      bodyAssumptions =
        assumptions (snd body)
      constraints' =
        Set.unions $ flip map (zip vars typeVars) $ \(var, typeVar) ->
          case Map.lookup var bodyAssumptions of
            Nothing -> Set.empty
            Just assumption -> Set.map (\typ -> EqualityConstraint typeVar typ) assumption
      assumptions' =
        foldr Map.delete bodyAssumptions vars
      res =
        Result (constraints (snd body) `mappend` constraints') assumptions'
    pure (Fix (Type.Function typeVars (fst body)), res)


generateConstraints :: Monad m =>
  TermF TypeId TermId (Type TypeId, Result TypeId TermId) ->
  UniqueT m (Type TypeId, Result TypeId TermId)
generateConstraints termF =
  case termF of

    Term.Constant constant ->
      generateConstraintsForConstant constant

    Term.Variable uniqueId ->
      generateConstraintsForVariable uniqueId

    Term.Abstraction vars body ->
      generateConstraintsForAbstraction vars body


typeOfNumeric :: Term.Numeric -> Type.NumericPrimitive
typeOfNumeric numeric =
  case numeric of

    Term.NumericFloat (Term.BitWidth width) _ ->
      Type.FloatPrimitive (Type.BitWidth width)

    Term.NumericSigned (Term.BitWidth width) _ ->
      Type.IntegerPrimitive (Type.BitWidth width)

    Term.NumericUnsigned (Term.BitWidth width) _ ->
      Type.UnsignedPrimitive (Type.BitWidth width)


typeOfConstant :: Term.Constant -> Type.Primitive
typeOfConstant constant =
  case constant of

    Term.NumericConstant numeric ->
      Type.NumericPrimitive (typeOfNumeric numeric)

    Term.BooleanConstant _ ->
      Type.BooleanPrimitive

    Term.StringConstant _ ->
      Type.StringPrimitive

    Term.CharConstant _ ->
      Type.CharPrimitive
