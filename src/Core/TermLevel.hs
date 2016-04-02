module Core.TermLevel where

import Data.Map (Map)
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
