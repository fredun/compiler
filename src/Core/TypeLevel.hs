module Core.TypeLevel where

import GHC.Generics

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Identifier

data Constant =
    PrimitiveConstant
  | FunctionConstant
  | ForAllConstant Kind
  | ExistsConstant Kind
  | RecordConstant (Set Identifier)
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Constant

data Kind =
    KindOfTypes
  | KindOfTypeConstructors Kind Kind
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Kind

data TypeF t =
    Constant Constant
  | Variable Identifier
  | Abstraction Identifier t
  | Application t t
  deriving (Eq, Ord, Show, Functor, Generic)

instance Aeson.ToJSON t => Aeson.ToJSON (TypeF t)

newtype Type = Type (Mu TypeF)
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON Type where
  toJSON (Type fix) = Fix.cata Aeson.toJSON fix

instance Fix.EqF TypeF where equalF = (==)
instance Fix.OrdF TypeF where compareF = compare
instance Fix.ShowF TypeF where showsPrecF = showsPrec

kindOfConstant :: Constant -> Kind
kindOfConstant constant = case constant of

  -- Primitives (String, Int, etc.) have kind *.
  PrimitiveConstant ->
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

freeVarsF :: TypeF (Set Identifier) -> Set Identifier
freeVarsF typ = case typ of

  -- A constant doesn't have any free type variables.
  Constant _ ->
    Set.empty

  -- A type variable itself is inherently a free variable.
  Variable var ->
    Set.singleton var

  -- An abstraction creates a binding for a type variable,
  -- and thus eliminates a free type variable from its body.
  Abstraction var vars ->
    Set.delete var vars

  -- An application consists of two types, both of which
  -- can have free type variables.
  Application leftVars rightVars ->
    Set.union leftVars rightVars

freeVars :: Type -> Set Identifier
freeVars (Type mu) = Fix.cata freeVarsF mu
