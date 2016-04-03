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
freeVarsF Constant = Set.empty
freeVarsF (Variable v) = Set.singleton v
freeVarsF (Abstraction v t) = Set.delete v t
freeVarsF (Application l r) = Set.union l r
freeVarsF (TypeAbstraction _ t) = t
freeVarsF (TypeApplication t _) = t
freeVarsF (RecordIntroduction m) = Set.unions (Map.elems m)
freeVarsF (RecordElimination t v) = Set.insert v t

freeVars :: Term -> Set Text
freeVars = Fix.cata freeVarsF

freeTypeVarsF :: TermF (Set Text) -> Set Text
freeTypeVarsF Constant = Set.empty
freeTypeVarsF (Variable _) = Set.empty
freeTypeVarsF (Abstraction _ t) = t
freeTypeVarsF (Application l r) = Set.union l r
freeTypeVarsF (TypeAbstraction v t) = Set.delete v t
freeTypeVarsF (TypeApplication t y) = Set.union t (TypeLevel.freeVars y)
freeTypeVarsF (RecordIntroduction m) = Set.unions (Map.elems m)
freeTypeVarsF (RecordElimination t _) = t

freeTypeVars :: Term -> Set Text
freeTypeVars = Fix.cata freeVarsF
