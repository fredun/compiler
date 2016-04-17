module Core.Kinding where

import qualified Syntax.Type as Type


kindOfConstant :: Type.Constant -> Type.Kind
kindOfConstant constant =
  case constant of

    -- Primitives (String, Int, etc.) have kind *.
    Type.PrimitiveConstant _ ->
      Type.KindOfTypes

    -- Records have kind *.
    Type.RecordConstant _ ->
      Type.KindOfTypes
