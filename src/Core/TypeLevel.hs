module Core.TypeLevel where

import Data.Set (Set)
import Data.Text (Text)

data Constant =
    PrimitiveConstant
  | FunctionConstant Constant Constant
  | ForAllConstant Kind
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
