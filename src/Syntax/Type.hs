{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}

module Syntax.Type where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import Data.Set (Set)

import Data.Typeable
import Data.Data


newtype Identifier = Identifier String
  deriving (Eq, Ord, Show, Typeable, Data)


newtype BitWidth = BitWidth Integer
  deriving (Eq, Ord, Show, Typeable, Data)


data NumericPrimitive =
    FloatPrimitive BitWidth
  | IntegerPrimitive BitWidth
  | UnsignedPrimitive BitWidth
  deriving (Eq, Ord, Show, Typeable, Data)


data Primitive =
    StringPrimitive
  | NumericPrimitive NumericPrimitive
  | CharPrimitive
  | BooleanPrimitive
  deriving (Eq, Ord, Show, Typeable, Data)


data Constant =
    PrimitiveConstant Primitive
  | RecordConstant (Set Identifier)
  deriving (Eq, Ord, Show, Typeable, Data)


data Kind =
    KindOfTypes
  | KindOfTypeConstructors Kind Kind
  deriving (Eq, Ord, Show, Typeable, Data)


data TypeF id t =
    Constant Constant
  | Variable id
  | Function [t] t
  deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Data)

deriving instance Data id => Data (Mu (TypeF id))

instance Eq id => Fix.EqF (TypeF id) where equalF = (==)
instance Ord id => Fix.OrdF (TypeF id) where compareF = compare
instance Show id => Fix.ShowF (TypeF id) where showsPrecF = showsPrec


type Type id = Mu (TypeF id)
