module Core.TermLevel where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Core.TypeLevel as TypeLevel

data Term =
    Constant
  | Variable Text
  | Abstraction Text TypeLevel.Type Term
  | Application Term Term
  | TypeAbstraction Text TypeLevel.Kind Term
  | TypeApplication Term TypeLevel.Type
  | RecordIntroduction (Map Text Term)
  | RecordElimination Term Text

freeVars :: Term -> Set Text
freeVars Constant = Set.empty
freeVars (Variable v) = Set.singleton v
freeVars (Abstraction v _ t) = Set.delete v (freeVars t)
freeVars (Application l r) = Set.union (freeVars l) (freeVars r)
freeVars (TypeAbstraction _ _ t) = freeVars t
freeVars (TypeApplication t _) = freeVars t
freeVars (RecordIntroduction m) = Set.unions (map freeVars (Map.elems m))
freeVars (RecordElimination t v) = Set.insert v (freeVars t)

freeTypeVars :: Term -> Set Text
freeTypeVars Constant = Set.empty
freeTypeVars (Variable _) = Set.empty
freeTypeVars (Abstraction _ y t) = Set.union (TypeLevel.freeVars y) (freeTypeVars t)
freeTypeVars (Application l r) = Set.union (freeTypeVars l) (freeTypeVars r)
freeTypeVars (TypeAbstraction v _ t) = Set.delete v (freeTypeVars t)
freeTypeVars (TypeApplication t y) = Set.union (freeTypeVars t) (TypeLevel.freeVars y)
freeTypeVars (RecordIntroduction m) = Set.unions (map freeTypeVars (Map.elems m))
freeTypeVars (RecordElimination t _) = freeTypeVars t
