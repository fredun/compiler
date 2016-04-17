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


data TermF typeId id t =
    Constant Constant
  | Variable id
  | Abstraction [id] t
  | Application t [t]
  | TypeForAll typeId Type.Kind t
  | RecordIntroduction (Map String t)
  | RecordElimination t String
  | Operation (Operation t)
  deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Data)

deriving instance (Data typeId, Data id) => Data (Mu (TermF typeId id))

instance (Eq typeId, Eq id) => Fix.EqF (TermF typeId id) where equalF = (==)
instance (Ord typeId, Ord id) => Fix.OrdF (TermF typeId id) where compareF = compare
instance (Show typeId, Show id) => Fix.ShowF (TermF typeId id) where showsPrecF = showsPrec


type Term typeId id = Mu (TermF typeId id)
