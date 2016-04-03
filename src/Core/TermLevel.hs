module Core.TermLevel where

import Data.Fix (Fix)
import qualified Data.Fix as Fix
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Core.TypeLevel as TypeLevel

data TermF t =
    Constant
  | Variable Text
  | Abstraction Text t
  | Application t t
  | TypeAbstraction Text t
  | TypeApplication t TypeLevel.Type
  | RecordIntroduction (Map Text t)
  | RecordElimination t Text
  deriving (Functor)

type Term = Fix TermF

freeVarsF :: TermF (Set Text) -> Set Text
freeVarsF term = case term of

  Constant ->
    Set.empty

  Variable var ->
    Set.singleton var

  Abstraction var vars ->
    Set.delete var vars

  Application leftVars rightVars ->
    Set.union leftVars rightVars

  TypeAbstraction _ vars ->
    vars

  TypeApplication vars _ ->
    vars

  RecordIntroduction mapping ->
    Set.unions (Map.elems mapping)

  RecordElimination vars var ->
    Set.insert var vars

freeVars :: Term -> Set Text
freeVars = Fix.cata freeVarsF

freeTypeVarsF :: TermF (Set Text) -> Set Text
freeTypeVarsF term = case term of

  Constant ->
    Set.empty

  Variable _ ->
    Set.empty

  Abstraction _ typeVars ->
    typeVars

  Application leftVars rightVars ->
    Set.union leftVars rightVars

  TypeAbstraction typeVar typeVars ->
    Set.delete typeVar typeVars

  TypeApplication typeVars typ ->
    Set.union typeVars (TypeLevel.freeVars typ)

  RecordIntroduction mapping ->
    Set.unions (Map.elems mapping)

  RecordElimination typeVars _ ->
    typeVars

freeTypeVars :: Term -> Set Text
freeTypeVars = Fix.cata freeVarsF
