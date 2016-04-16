module Core.Kinding where

import qualified Syntax.Type as Type


kindOfConstant :: Type.Constant -> Type.Kind
kindOfConstant constant =
  case constant of

    -- Primitives (String, Int, etc.) have kind *.
    Type.PrimitiveConstant _ ->
      Type.KindOfTypes

    -- The function constructor has kind (* -> (* -> *)).
    -- It is a type constructor with two arguments
    -- which gets applied to both the codomain and the domain
    Type.FunctionConstant ->
      Type.KindOfTypeConstructors Type.KindOfTypes (Type.KindOfTypeConstructors Type.KindOfTypes Type.KindOfTypes)

    -- The forall(k) constructor has kind ((k -> *) -> *).
    -- It is a type constructor parameterised by a kind, that can be applied
    -- to a type lambda.
    Type.ForAllConstant kind ->
      Type.KindOfTypeConstructors (Type.KindOfTypeConstructors kind Type.KindOfTypes) Type.KindOfTypes

    -- The exists(k) constructor has kind ((k -> *) -> *).
    -- It is a type constructor parameterised by a kind, that can be applied
    -- to a type lambda.
    Type.ExistsConstant kind ->
      Type.KindOfTypeConstructors (Type.KindOfTypeConstructors kind Type.KindOfTypes) Type.KindOfTypes

    -- Records have kind *.
    Type.RecordConstant _ ->
      Type.KindOfTypes
