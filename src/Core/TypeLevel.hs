module Core.TypeLevel where

import Data.Fix (Fix)
import qualified Data.Fix as Fix
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

data Constant =
    PrimitiveConstant
  | FunctionConstant
  | ForAllConstant Kind
  | ExistsConstant Kind
  | RecordConstant (Set Text)

data Kind =
    KindOfTypes
  | KindOfTypeConstructors Kind Kind

data TypeF t =
    Constant Constant
  | Variable Text
  | Abstraction Text t
  | Application t t
  deriving (Functor)

type Type = Fix TypeF

data Binding =
    TypeVariableBinding Text Kind
  | TermVariableBinding Text Type

kindOfConstant :: Constant -> Kind
kindOfConstant constant = case constant of

  PrimitiveConstant ->
    KindOfTypes

  FunctionConstant ->
    KindOfTypeConstructors KindOfTypes (KindOfTypeConstructors KindOfTypes KindOfTypes)

  ForAllConstant kind ->
    KindOfTypeConstructors (KindOfTypeConstructors kind KindOfTypes) KindOfTypes

  ExistsConstant kind ->
    KindOfTypeConstructors (KindOfTypeConstructors kind KindOfTypes) KindOfTypes

  RecordConstant _ ->
    KindOfTypes

freeVarsF :: TypeF (Set Text) -> Set Text
freeVarsF typ = case typ of

  Constant _ ->
    Set.empty

  Variable var ->
    Set.singleton var

  Abstraction var vars ->
    Set.delete var vars

  Application leftVars rightVars ->
    Set.union leftVars rightVars

freeVars :: Type -> Set Text
freeVars = Fix.cata freeVarsF
