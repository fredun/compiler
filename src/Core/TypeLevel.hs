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
kindOfConstant PrimitiveConstant = KindOfTypes
kindOfConstant FunctionConstant = KindOfTypeConstructors KindOfTypes (KindOfTypeConstructors KindOfTypes KindOfTypes)
kindOfConstant (ForAllConstant k) = KindOfTypeConstructors (KindOfTypeConstructors k KindOfTypes) KindOfTypes
kindOfConstant (ExistsConstant k) = KindOfTypeConstructors (KindOfTypeConstructors k KindOfTypes) KindOfTypes
kindOfConstant (RecordConstant _) = KindOfTypes

freeVarsF :: TypeF (Set Text) -> Set Text
freeVarsF (Constant _) = Set.empty
freeVarsF (Variable v) = Set.singleton v
freeVarsF (Abstraction v t) = Set.delete v t
freeVarsF (Application l r) = Set.union l r

freeVars :: Type -> Set Text
freeVars = Fix.cata freeVarsF
