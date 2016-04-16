module Core.Typing where

import qualified Syntax.Type as Type
import qualified Syntax.Term as Term


typeOfNumeric :: Term.Numeric -> Type.NumericPrimitive
typeOfNumeric numeric =
  case numeric of

    Term.NumericFloat (Term.BitWidth width) _ ->
      Type.FloatPrimitive (Type.BitWidth width)

    Term.NumericSigned (Term.BitWidth width) _ ->
      Type.IntegerPrimitive (Type.BitWidth width)

    Term.NumericUnsigned (Term.BitWidth width) _ ->
      Type.UnsignedPrimitive (Type.BitWidth width)


typeOfConstant :: Term.Constant -> Type.Primitive
typeOfConstant constant =
  case constant of

    Term.NumericConstant numeric ->
      Type.NumericPrimitive (typeOfNumeric numeric)

    Term.BooleanConstant _ ->
      Type.BooleanPrimitive

    Term.StringConstant _ ->
      Type.StringPrimitive

    Term.CharConstant _ ->
      Type.CharPrimitive
