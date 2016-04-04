module Core.TermLevel where

import Data.Fix (Fix)
import qualified Data.Fix as Fix
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Core.TypeLevel as TypeLevel

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show)

data TermF t =
    Constant
  | Variable Identifier
  | Abstraction Identifier t
  | Application t t
  | TypeAbstraction TypeLevel.Identifier t
  | TypeApplication t TypeLevel.Type
  | RecordIntroduction (Map Identifier t)
  | RecordElimination t Identifier
  deriving (Functor)

type Term = Fix TermF

freeVarsF :: TermF (Set Identifier) -> Set Identifier
freeVarsF term = case term of

  -- A constant does not have any free variables.
  Constant ->
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
freeVars = Fix.cata freeVarsF

freeTypeVarsF :: TermF (Set TypeLevel.Identifier) -> Set TypeLevel.Identifier
freeTypeVarsF term = case term of

  -- A constant doesn't have any free type variables.
  Constant ->
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
freeTypeVars = Fix.cata freeTypeVarsF
