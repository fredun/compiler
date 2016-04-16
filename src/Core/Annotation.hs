module Core.Annotation where

import qualified Data.Generics.Fixplate as Fix

import Data.Set (Set)


data Annotation typeId = Annotation
  { freeVars :: Set typeId
  }
  deriving (Eq, Ord, Show)


type Annotated f id = Fix.Attr (f id) (Annotation id)
