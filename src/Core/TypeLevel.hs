module Core.TypeLevel where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show)

data Constant =
    PrimitiveConstant
  | FunctionConstant
  | ForAllConstant Kind
  | ExistsConstant Kind
  | RecordConstant (Set Identifier)
  deriving (Eq, Ord, Show)

data Kind =
    KindOfTypes
  | KindOfTypeConstructors Kind Kind
  deriving (Eq, Ord, Show)

data TypeF t =
    Constant Constant
  | Variable Identifier
  | Abstraction Identifier t
  | Application t t
  deriving (Eq, Ord, Show, Functor)

instance Fix.EqF TypeF where equalF = (==)
instance Fix.OrdF TypeF where compareF = compare
instance Fix.ShowF TypeF where showsPrecF = showsPrec

type Type = Mu TypeF

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
freeVars = Fix.cata freeVarsF
