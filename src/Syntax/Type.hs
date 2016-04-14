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
  | FunctionConstant
  | ForAllConstant Kind
  | ExistsConstant Kind
  | RecordConstant (Set Identifier)
  deriving (Eq, Ord, Show, Typeable, Data)


data Kind =
    KindOfTypes
  | KindOfTypeConstructors Kind Kind
  deriving (Eq, Ord, Show, Typeable, Data)


data TypeF t =
    Constant Constant
  | Variable Identifier
  | Abstraction Identifier t
  | Application t t
  deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Data)

deriving instance Data (Mu TypeF)

instance Fix.EqF TypeF where equalF = (==)
instance Fix.OrdF TypeF where compareF = compare
instance Fix.ShowF TypeF where showsPrecF = showsPrec


newtype Type = Type (Mu TypeF)
  deriving (Eq, Ord, Show, Typeable, Data)


kindOfConstant :: Constant -> Kind
kindOfConstant constant = case constant of

  -- Primitives (String, Int, etc.) have kind *.
  PrimitiveConstant _ ->
    KindOfTypes

  -- The function constructor has kind (* -> (* -> *)).
  -- It is a type constructor with two arguments
  -- which gets applied to both the codomain and the domain
  FunctionConstant ->
    KindOfTypeConstructors KindOfTypes (KindOfTypeConstructors KindOfTypes KindOfTypes)

  -- The forall(k) constructor has kind ((k -> *) -> *).
  -- It is a type constructor parameterised by a kind, that can be applied
  -- to a type lambda.
  ForAllConstant kind ->
    KindOfTypeConstructors (KindOfTypeConstructors kind KindOfTypes) KindOfTypes

  -- The exists(k) constructor has kind ((k -> *) -> *).
  -- It is a type constructor parameterised by a kind, that can be applied
  -- to a type lambda.
  ExistsConstant kind ->
    KindOfTypeConstructors (KindOfTypeConstructors kind KindOfTypes) KindOfTypes

  -- Records have kind *.
  RecordConstant _ ->
    KindOfTypes
