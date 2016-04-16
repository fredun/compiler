module Core.Annotation where

import qualified Data.Generics.Fixplate as Fix

import Data.Set (Set)

import Syntax.Type (TypeF)
import qualified Syntax.Type as Type


data TypeAnnotation = TypeAnnotation
  { freeTypeVars :: Set Type.Identifier
  }
  deriving (Eq, Ord, Show)


type AnnotatedType = Fix.Attr TypeF TypeAnnotation
