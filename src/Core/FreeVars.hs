module Core.FreeVars where

import qualified Data.Generics.Fixplate as Fix

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Syntax.Type (Type(..))
import qualified Syntax.Type as Type

import Syntax.Term (Term(..))
import qualified Syntax.Term as Term


freeVarsOp :: Ord a => Term.Operation (Set a) -> Set a
freeVarsOp opn =
  case opn of

    -- A binary operation has a left and right
    -- argument. Both arguments may contain free variables.
    Term.BinaryOperation _ left right ->
      Set.union left right

    -- A unary operation has an argument which may contain
    -- free variables.
    Term.UnaryOperation _ vars ->
      vars


freeVarsF :: Term.TermF (Set Term.Identifier) -> Set Term.Identifier
freeVarsF term =
  case term of

    -- A constant does not have any free variables.
    Term.Constant _ ->
      Set.empty

    -- A variable itself is inherently a free variable.
    Term.Variable var ->
      Set.singleton var

    -- An operation may contain free variables in its arguments.
    Term.Operation op ->
      freeVarsOp op

    -- An abstraction creates binding for variables,
    -- and thus eliminates free variables from its body.
    Term.Abstraction argVars vars ->
      Set.difference vars (Set.fromList argVars)

    -- An application consists of a body term,
    -- and multiple argument terms, all of which
    -- can have free variables.
    Term.Application leftVars rightVars ->
      Set.union leftVars (Set.unions rightVars)

    -- A type abstration has a body that might contain free variables.
    Term.TypeAbstraction _ vars ->
      vars

    -- A type application has a body that might contain free variables.
    Term.TypeApplication vars _ ->
      vars

    -- A record introduction has a term per record field, all of which
    -- might contain free variables.
    Term.RecordIntroduction mapping ->
      Set.unions (Map.elems mapping)

    -- A record elimination has a body that might contain free variables.
    Term.RecordElimination vars _ ->
      vars


freeVars :: Term -> Set Term.Identifier
freeVars (Term mu) =
  Fix.cata freeVarsF mu


freeTypeVarsF :: Term.TermF (Set Type.Identifier) -> Set Type.Identifier
freeTypeVarsF term =
  case term of

    -- A constant doesn't have any free type variables.
    Term.Constant _ ->
      Set.empty

    -- A type variable itself is inherently a free variable.
    Term.Variable _ ->
      Set.empty

    -- An operation may contain free type variables in its arguments.
    Term.Operation op ->
      freeVarsOp op

    -- An abstraction has a body that might contain free type variables.
    Term.Abstraction _ typeVars ->
      typeVars

    -- An application consists of two terms, both of which
    -- can have free type variables.
    Term.Application leftVars rightVars ->
      Set.union leftVars (Set.unions rightVars)

    -- A type abstraction creates a binding for a type variable,
    -- and thus eliminates a free type variable from its body.
    Term.TypeAbstraction argTypeVars typeVars ->
      Set.difference typeVars (Set.fromList argTypeVars)

    -- A type application consists of a term and a type, both of which
    -- can have free type variables.
    Term.TypeApplication typeVars types ->
      Set.union typeVars (Set.unions (map freeVarsType types))

    -- A record introduction has a term per record field, all of which
    -- might contain free type variables.
    Term.RecordIntroduction mapping ->
      Set.unions (Map.elems mapping)

    -- A record elimination has a body that might contain free type variables.
    Term.RecordElimination typeVars _ ->
      typeVars


freeTypeVars :: Term -> Set Type.Identifier
freeTypeVars (Term mu) =
  Fix.cata freeTypeVarsF mu


freeVarsTypeF :: Type.TypeF (Set Type.Identifier) -> Set Type.Identifier
freeVarsTypeF typeF =
  case typeF of

    -- A constant doesn't have any free type variables.
    Type.Constant _ ->
      Set.empty

    -- A type variable itself is inherently a free variable.
    Type.Variable var ->
      Set.singleton var

    -- An abstraction creates a binding for a type variable,
    -- and thus eliminates a free type variable from its body.
    Type.Abstraction var vars ->
      Set.delete var vars

    -- An application consists of two types, both of which
    -- can have free type variables.
    Type.Application leftVars rightVars ->
      Set.union leftVars rightVars


freeVarsType :: Type -> Set Type.Identifier
freeVarsType (Type mu) =
  Fix.cata freeVarsTypeF mu
