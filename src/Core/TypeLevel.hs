module Core.TypeLevel where

import Data.Set (Set)
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

data Type =
    Constant Constant
  | Variable Text
  | Abstraction Text Kind Type
  | Application Type Type

data Binding =
    TypeVariableBinding Text Kind
  | TermVariableBinding Text Type

kindOfConstant :: Constant -> Kind
kindOfConstant PrimitiveConstant = KindOfTypes
kindOfConstant FunctionConstant = KindOfTypeConstructors KindOfTypes (KindOfTypeConstructors KindOfTypes KindOfTypes)
kindOfConstant (ForAllConstant k) = KindOfTypeConstructors (KindOfTypeConstructors k KindOfTypes) KindOfTypes
kindOfConstant (ExistsConstant k) = KindOfTypeConstructors (KindOfTypeConstructors k KindOfTypes) KindOfTypes
kindOfConstant (RecordConstant _) = KindOfTypes
