module Core.Unification where

import Data.Generics.Fixplate (Mu(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax.Type (Type)
import qualified Syntax.Type as Type


mostGeneralUnifier :: Type -> Type -> Maybe (Map Type.Identifier Type)
mostGeneralUnifier (Fix left) (Fix right) =
  case (left, right) of

    (Type.Variable leftVar, _) ->
      Just (Map.singleton leftVar (Fix right))

    (_, Type.Variable rightVar) ->
      Just (Map.singleton rightVar (Fix left))

    (Type.Constant leftConst, Type.Constant rightConst) ->
      if leftConst == rightConst
        then Just Map.empty
        else Nothing

    _ ->
      Nothing


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

    Type.Abstraction arg body ->
      let
        bodySubs = Map.delete arg subs
      in
        Fix (Type.Abstraction arg (substitute body bodySubs))

    Type.Application body arg ->
      Fix (Type.Application (substitute body subs) (substitute arg subs))
