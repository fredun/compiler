{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}

module Core.TermLevel where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Scientific (Scientific)
import qualified Data.Map as Map

import Data.Typeable
import Data.Data

import qualified Core.TypeLevel as TypeLevel

newtype Identifier = Identifier String
  deriving (Eq, Ord, Show, Typeable, Data)

newtype BitWidth = BitWidth Integer
  deriving (Eq, Ord, Show, Typeable, Data)

data Constant =
    IntegerConstant BitWidth Integer
  | ScientificConstant BitWidth Scientific
  | StringConstant String
  | CharConstant Char
  | BooleanConstant Bool
  deriving (Eq, Ord, Show, Typeable, Data)

data TermF t =
    Constant Constant
  | Variable Identifier
  | Abstraction [Identifier] t
  | Application t [t]
  | TypeAbstraction [TypeLevel.Identifier] t
  | TypeApplication t [TypeLevel.Type]
  | RecordIntroduction (Map String t)
  | RecordElimination t Identifier
  deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Data)

deriving instance Data (Mu TermF)

instance Fix.EqF TermF where equalF = (==)
instance Fix.OrdF TermF where compareF = compare
instance Fix.ShowF TermF where showsPrecF = showsPrec

newtype Term = Term (Mu TermF)
  deriving (Eq, Ord, Show, Typeable, Data)

freeVarsF :: TermF (Set Identifier) -> Set Identifier
freeVarsF term = case term of

  -- A constant does not have any free variables.
  Constant _ ->
    Set.empty

  -- A variable itself is inherently a free variable.
  Variable var ->
    Set.singleton var

  -- An abstraction creates binding for variables,
  -- and thus eliminates free variables from its body.
  Abstraction argVars vars ->
    Set.difference vars (Set.fromList argVars)

  -- An application consists of a body term,
  -- and multiple argument terms, all of which
  -- can have free variables.
  Application leftVars rightVars ->
    Set.union leftVars (Set.unions rightVars)

  -- A type abstration has a body that might contain free variables.
  TypeAbstraction _ vars ->
    vars

  -- A type application has a body that might contain free variables.
  TypeApplication vars _ ->
    vars

  -- A record introduction has a term per record field, all of which
  -- might contain free variables.
  RecordIntroduction mapping ->
    Set.unions (Map.elems mapping)

  -- A record elimination has a body that might contain free variables.
  RecordElimination vars _ ->
    vars

freeVars :: Term -> Set Identifier
freeVars (Term mu) = Fix.cata freeVarsF mu

freeTypeVarsF :: TermF (Set TypeLevel.Identifier) -> Set TypeLevel.Identifier
freeTypeVarsF term = case term of

  -- A constant doesn't have any free type variables.
  Constant _ ->
    Set.empty

  -- A type variable itself is inherently a free variable.
  Variable _ ->
    Set.empty

  -- An abstraction has a body that might contain free type variables.
  Abstraction _ typeVars ->
    typeVars

  -- An application consists of two terms, both of which
  -- can have free type variables.
  Application leftVars rightVars ->
    Set.union leftVars (Set.unions rightVars)

  -- A type abstraction creates a binding for a type variable,
  -- and thus eliminates a free type variable from its body.
  TypeAbstraction argTypeVars typeVars ->
    Set.difference typeVars (Set.fromList argTypeVars)

  -- A type application consists of a term and a type, both of which
  -- can have free type variables.
  TypeApplication typeVars types ->
    Set.union typeVars (Set.unions (map TypeLevel.freeVars types))

  -- A record introduction has a term per record field, all of which
  -- might contain free type variables.
  RecordIntroduction mapping ->
    Set.unions (Map.elems mapping)

  -- A record elimination has a body that might contain free type variables.
  RecordElimination typeVars _ ->
    typeVars

freeTypeVars :: Term -> Set TypeLevel.Identifier
freeTypeVars (Term mu) = Fix.cata freeTypeVarsF mu
