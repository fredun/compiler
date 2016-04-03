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
