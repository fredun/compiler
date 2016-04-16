module Core.Substitution where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Syntax.Type as Type

import Core.Annotation (AnnotatedType)


type TypeSubstitution = Map Type.Identifier AnnotatedType


substituteType :: AnnotatedType -> TypeSubstitution -> AnnotatedType
substituteType (Fix (Fix.Ann ann typeF)) subs =
  case typeF of

    Type.Constant c ->
      Fix (Fix.Ann ann (Type.Constant c))

    Type.Variable var ->
      case Map.lookup var subs of
        Just sub ->
          substituteType sub subs
        Nothing ->
          Fix (Fix.Ann ann (Type.Variable var))

    Type.Abstraction arg kind body ->
      let
        bodySubs = Map.delete arg subs
      in
        Fix (Fix.Ann ann (Type.Abstraction arg kind (substituteType body bodySubs)))

    Type.Application body arg ->
      Fix (Fix.Ann ann (Type.Application (substituteType body subs) (substituteType arg subs)))
