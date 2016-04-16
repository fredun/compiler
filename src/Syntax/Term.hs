{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}

module Syntax.Term where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import Data.Map (Map)
import Data.Scientific (Scientific)

import Data.Typeable
import Data.Data

import Syntax.Type (Type)
import qualified Syntax.Type as Type


newtype Identifier = Identifier String
  deriving (Eq, Ord, Show, Typeable, Data)


newtype BitWidth = BitWidth Integer
  deriving (Eq, Ord, Show, Typeable, Data)


data Numeric =
    NumericFloat BitWidth Scientific
  | NumericSigned BitWidth Integer
  | NumericUnsigned BitWidth Integer
  deriving (Eq, Ord, Show, Typeable, Data)


data Constant =
    NumericConstant Numeric
  | StringConstant String
  | CharConstant Char
  | BooleanConstant Bool
  deriving (Eq, Ord, Show, Typeable, Data)


newtype Operator = Operator String
  deriving (Eq, Ord, Show, Typeable, Data)


data Operation t =
    BinaryOperation Operator t t
  | UnaryOperation Operator t
  deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Data)


data TermF t =
    Constant Constant
  | Variable Identifier
  | Abstraction [Identifier] t
  | Application t [t]
  | TypeAbstraction [(Type.Identifier, Type.Kind)] t
  | TypeApplication t [Type]
  | RecordIntroduction (Map String t)
  | RecordElimination t Identifier
  | Operation (Operation t)
  deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Data)

deriving instance Data (Mu TermF)

instance Fix.EqF TermF where equalF = (==)
instance Fix.OrdF TermF where compareF = compare
instance Fix.ShowF TermF where showsPrecF = showsPrec


type Term = Mu TermF
