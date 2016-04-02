module Core.TypeLevel where

import Data.Set (Set)
import Data.Text (Text)

data Constant =
    PrimitiveConstant
  | FunctionConstant Constant Constant
  | RecordConstant (Set Text)

data Kind =
    KindOfTypes
  | KindOfTypeConstructors

data Type =
    Constant Constant
  | Variable Text
  | Abstraction Text Kind Type
  | Application Type Type
