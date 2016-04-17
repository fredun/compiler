module Core.FreeVars where

import qualified Data.Generics.Fixplate as Fix

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Syntax.Type (Type, TypeF)
import qualified Syntax.Type as Type

import Syntax.Term (Term, TermF)
import qualified Syntax.Term as Term


freeVarsOp :: Ord id => Term.Operation (Set id) -> Set id
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


freeVarsF :: Ord id => TermF typeId id (Set id) -> Set id
freeVarsF termF =
  case termF of

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
    Term.TypeForAll _ _ vars ->
      vars

    -- A record introduction has a term per record field, all of which
    -- might contain free variables.
    Term.RecordIntroduction mapping ->
      Set.unions (Map.elems mapping)

    -- A record elimination has a body that might contain free variables.
    Term.RecordElimination vars _ ->
      vars


freeVars :: Ord id => Term typeId id -> Set id
freeVars mu =
  Fix.cata freeVarsF mu


freeTypeVarsF :: Ord typeId => TermF typeId id (Set typeId) -> Set typeId
freeTypeVarsF termF =
  case termF of

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
    Term.TypeForAll typeVar _ typeVars ->
      Set.delete typeVar typeVars

    -- A record introduction has a term per record field, all of which
    -- might contain free type variables.
    Term.RecordIntroduction mapping ->
      Set.unions (Map.elems mapping)

    -- A record elimination has a body that might contain free type variables.
    Term.RecordElimination typeVars _ ->
      typeVars


freeTypeVars :: Ord typeId => Term typeId id -> Set typeId
freeTypeVars mu =
  Fix.cata freeTypeVarsF mu


freeVarsTypeF :: Ord typeId => TypeF typeId (Set typeId) -> Set typeId
freeVarsTypeF typeF =
  case typeF of

    -- A constant doesn't have any free type variables.
    Type.Constant _ ->
      Set.empty

    -- A type variable itself is inherently a free variable.
    Type.Variable var ->
      Set.singleton var

    -- An function consists of multiple types, both in its
    -- arguments and in its body, all of which can have free
    -- type variables.
    Type.Function args body ->
      Set.union (Set.unions args) body

    -- A universal quantification creates a binding for a type variable,
    -- and thus eliminates a free type variable from its body.
    Type.ForAll var _ vars ->
      Set.delete var vars


freeVarsType :: Ord typeId => Type typeId -> Set typeId
freeVarsType mu =
  Fix.cata freeVarsTypeF mu


annotateFreeVarsType :: Ord typeId => Type typeId -> Fix.Attr (TypeF typeId) (Set typeId)
annotateFreeVarsType mu =
  Fix.synthetise freeVarsTypeF mu
