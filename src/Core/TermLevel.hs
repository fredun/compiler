{-# LANGUAGE DeriveDataTypeable #-}

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

data Constant =
    NumericConstant (Either Integer Scientific)
  | StringConstant String
  | CharConstant Char
  | BooleanConstant Bool
  deriving (Eq, Ord, Show, Typeable, Data)

data TermF t =
    Constant Constant
  | Variable Identifier
  | Abstraction Identifier t
  | Application t t
  | TypeAbstraction TypeLevel.Identifier t
  | TypeApplication t TypeLevel.Type
  | RecordIntroduction (Map String t)
  | RecordElimination t Identifier
  deriving (Eq, Ord, Show, Functor, Typeable, Data)

newtype Term = Term (TermF Term)
  deriving (Eq, Ord, Show, Typeable, Data)

toMu :: Term -> Mu TermF
toMu (Term tf) = Fix (fmap toMu tf)

instance Fix.EqF TermF where equalF = (==)
instance Fix.OrdF TermF where compareF = compare
instance Fix.ShowF TermF where showsPrecF = showsPrec

freeVarsF :: TermF (Set Identifier) -> Set Identifier
freeVarsF term = case term of

  -- A constant does not have any free variables.
  Constant _ ->
    Set.empty

  -- A variable itself is inherently a free variable.
  Variable var ->
    Set.singleton var

  -- An abstraction creates a binding for a variable,
  -- and thus eliminates a free variable from its body.
  Abstraction var vars ->
    Set.delete var vars

  -- An application consists of two terms, both of which
  -- can have free variables.
  Application leftVars rightVars ->
    Set.union leftVars rightVars

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
freeVars t = Fix.cata freeVarsF (toMu t)

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
    Set.union leftVars rightVars

  -- A type abstraction creates a binding for a type variable,
  -- and thus eliminates a free type variable from its body.
  TypeAbstraction typeVar typeVars ->
    Set.delete typeVar typeVars

  -- A type application consists of a term and a type, both of which
  -- can have free type variables.
  TypeApplication typeVars typ ->
    Set.union typeVars (TypeLevel.freeVars typ)

  -- A record introduction has a term per record field, all of which
  -- might contain free type variables.
  RecordIntroduction mapping ->
    Set.unions (Map.elems mapping)

  -- A record elimination has a body that might contain free type variables.
  RecordElimination typeVars _ ->
    typeVars

freeTypeVars :: Term -> Set TypeLevel.Identifier
freeTypeVars t = Fix.cata freeTypeVarsF (toMu t)
