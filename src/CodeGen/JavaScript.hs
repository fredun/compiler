module CodeGen.JavaScript where

import qualified Data.Generics.Fixplate as Fix

import qualified Language.JavaScript.Parser.AST as JS
import qualified Language.JavaScript.Pretty.Printer as JS

import qualified Syntax.Term as Term


genBoolean :: Bool -> JS.JSExpression
genBoolean b =
  case b of
    True ->
      JS.JSIdentifier JS.JSNoAnnot "true"
    False ->
      JS.JSIdentifier JS.JSNoAnnot "false"


genNumeric :: Term.Numeric -> JS.JSExpression
genNumeric numeric =
  case numeric of

    Term.NumericFloat _ scientific ->
      JS.JSDecimal JS.JSNoAnnot (show scientific)

    Term.NumericSigned _ integer ->
      JS.JSDecimal JS.JSNoAnnot (show integer)

    Term.NumericUnsigned _ integer ->
      JS.JSDecimal JS.JSNoAnnot (show integer)


genPowOp :: JS.JSExpression -> JS.JSExpression -> JS.JSExpression
genPowOp left right =
  JS.JSCallExpressionDot
    (JS.JSIdentifier JS.JSNoAnnot "Math")
    JS.JSNoAnnot
    (
      JS.JSCallExpression
        (JS.JSIdentifier JS.JSNoAnnot "pow")
        JS.JSNoAnnot
        (genCommaList [left, right])
        JS.JSNoAnnot
    )


genBinOp :: Term.Operator -> JS.JSBinOp
genBinOp (Term.Operator op) =
  case op of
    "+" -> JS.JSBinOpPlus JS.JSNoAnnot
    "-" -> JS.JSBinOpMinus JS.JSNoAnnot
    "*" -> JS.JSBinOpTimes JS.JSNoAnnot
    "/" -> JS.JSBinOpDivide JS.JSNoAnnot
    "%" -> JS.JSBinOpMod JS.JSNoAnnot
    "<=" -> JS.JSBinOpLe JS.JSNoAnnot
    "<" -> JS.JSBinOpLt JS.JSNoAnnot
    ">=" -> JS.JSBinOpGe JS.JSNoAnnot
    ">" -> JS.JSBinOpGt JS.JSNoAnnot
    "==" -> JS.JSBinOpStrictEq JS.JSNoAnnot
    "!=" -> JS.JSBinOpStrictNeq JS.JSNoAnnot
    "&&" -> JS.JSBinOpAnd JS.JSNoAnnot
    "||" -> JS.JSBinOpOr JS.JSNoAnnot
    _ -> error "unknown binary operator"


genUnOp :: Term.Operator -> JS.JSUnaryOp
genUnOp (Term.Operator op) =
  case op of
    "+" -> JS.JSUnaryOpPlus JS.JSNoAnnot
    "-" -> JS.JSUnaryOpMinus JS.JSNoAnnot
    "!" -> JS.JSUnaryOpNot JS.JSNoAnnot
    _ -> error "unknown binary operator"


genConstant :: Term.Constant -> JS.JSExpression
genConstant constant =
  case constant of

    Term.StringConstant s ->
      JS.JSStringLiteral JS.JSNoAnnot s

    Term.CharConstant c ->
      JS.JSStringLiteral JS.JSNoAnnot [c]

    Term.BooleanConstant b ->
      genBoolean b

    Term.NumericConstant n ->
      genNumeric n


genIdentifier :: Term.Identifier -> JS.JSExpression
genIdentifier (Term.Identifier identifier) =
  JS.JSIdentifier JS.JSNoAnnot identifier


genIdent :: Term.Identifier -> JS.JSIdent
genIdent (Term.Identifier identifier) =
  JS.JSIdentName JS.JSNoAnnot identifier


genArgs :: [Term.Identifier] -> JS.JSCommaList JS.JSIdent
genArgs list = genCommaList (map genIdent list)


genCommaList :: [a] -> JS.JSCommaList a
genCommaList list =
  case list of
    [] -> JS.JSLNil
    [expr] -> JS.JSLOne expr
    _ -> JS.JSLCons (genCommaList (init list)) JS.JSNoAnnot (last list)


genBody :: JS.JSExpression -> JS.JSBlock
genBody expr =
  JS.JSBlock JS.JSNoAnnot [stmt] JS.JSNoAnnot
  where
    stmt =
      JS.JSReturn JS.JSNoAnnot (Just expr) (JS.JSSemi JS.JSNoAnnot)


genOperation :: Term.Operation JS.JSExpression -> JS.JSExpression
genOperation opn =
  case opn of

    Term.UnaryOperation op arg ->
      JS.JSUnaryExpression (genUnOp op) arg

    Term.BinaryOperation op left right ->
      case op of
        (Term.Operator "^") -> genPowOp left right
        _ -> JS.JSExpressionBinary left (genBinOp op) right


genTermF :: Term.TermF JS.JSExpression -> JS.JSExpression
genTermF termF =
  case termF of

    Term.Constant constant ->
      genConstant constant

    Term.Variable identifier ->
      genIdentifier identifier

    Term.Operation opn ->
      genOperation opn

    Term.Abstraction args body ->
      JS.JSFunctionExpression
        JS.JSNoAnnot
        JS.JSIdentNone
        JS.JSNoAnnot
        (genArgs args)
        JS.JSNoAnnot
        (genBody body)

    Term.Application left rights ->
      JS.JSCallExpression
        left
        JS.JSNoAnnot
        (genCommaList rights)
        JS.JSNoAnnot

    Term.TypeAbstraction _ _ ->
      error "not implemented yet"

    Term.TypeApplication _ _ ->
      error "not implemented yet"

    Term.RecordIntroduction _ ->
      error "not implemented yet"

    Term.RecordElimination _ _ ->
      error "not implemented yet"


genTermF' :: Term.TermF JS.JSExpression -> JS.JSExpression
genTermF' termF =
  JS.JSExpressionParen JS.JSNoAnnot (genTermF termF) JS.JSNoAnnot


genTerm :: Term.Term -> JS.JSExpression
genTerm (Term.Term mu) =
  Fix.cata genTermF' mu


renderTerm :: Term.Term -> String
renderTerm term =
  JS.renderToString (JS.JSAstExpression (genTerm term) JS.JSNoAnnot)
