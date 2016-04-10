module CodeGen.JavaScript where

import Data.Generics.Fixplate (Mu(..))

import qualified Language.JavaScript.Parser.AST as JS
import qualified Language.JavaScript.Pretty.Printer as JS

import qualified Core.TermLevel as TermLevel


genBoolean :: Bool -> JS.JSExpression
genBoolean b =
  case b of
    True ->
      JS.JSIdentifier JS.JSNoAnnot "true"
    False ->
      JS.JSIdentifier JS.JSNoAnnot "false"


genNumeric :: TermLevel.Numeric -> JS.JSExpression
genNumeric numeric =
  case numeric of

    TermLevel.NumericFloat _ scientific ->
      JS.JSDecimal JS.JSNoAnnot (show scientific)

    TermLevel.NumericSigned _ integer ->
      JS.JSDecimal JS.JSNoAnnot (show integer)

    TermLevel.NumericUnsigned _ integer ->
      JS.JSDecimal JS.JSNoAnnot (show integer)


genBinOp :: TermLevel.Operator -> JS.JSBinOp
genBinOp (TermLevel.Operator op) =
  case op of
    "+" -> JS.JSBinOpPlus JS.JSNoAnnot
    "-" -> JS.JSBinOpMinus JS.JSNoAnnot
    "*" -> JS.JSBinOpTimes JS.JSNoAnnot
    "/" -> JS.JSBinOpDivide JS.JSNoAnnot
    _ -> error "unknown binary operator"


genUnOp :: TermLevel.Operator -> JS.JSUnaryOp
genUnOp (TermLevel.Operator op) =
  case op of
    "-" -> JS.JSUnaryOpMinus JS.JSNoAnnot
    "!" -> JS.JSUnaryOpNot JS.JSNoAnnot
    _ -> error "unknown binary operator"


genConstant :: TermLevel.Constant -> JS.JSExpression
genConstant constant =
  case constant of

    TermLevel.StringConstant s ->
      JS.JSStringLiteral JS.JSNoAnnot s

    TermLevel.CharConstant c ->
      JS.JSStringLiteral JS.JSNoAnnot [c]

    TermLevel.BooleanConstant b ->
      genBoolean b

    TermLevel.NumericConstant n ->
      genNumeric n


genIdentifier :: TermLevel.Identifier -> JS.JSExpression
genIdentifier (TermLevel.Identifier identifier) =
  JS.JSIdentifier JS.JSNoAnnot identifier


genIdent :: TermLevel.Identifier -> JS.JSIdent
genIdent (TermLevel.Identifier identifier) =
  JS.JSIdentName JS.JSNoAnnot identifier


genArgs :: [TermLevel.Identifier] -> JS.JSCommaList JS.JSIdent
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


genOperation :: (t -> JS.JSExpression) -> TermLevel.Operation t -> JS.JSExpression
genOperation genT opn =
  case opn of

    TermLevel.UnaryOperation op arg ->
      JS.JSUnaryExpression (genUnOp op) (genT arg)

    TermLevel.BinaryOperation op left right ->
      JS.JSExpressionBinary (genT left) (genBinOp op) (genT right)


genTermF :: (t -> JS.JSExpression) -> TermLevel.TermF t -> JS.JSExpression
genTermF genT termF =
  case termF of

    TermLevel.Constant constant ->
      genConstant constant

    TermLevel.Variable identifier ->
      genIdentifier identifier

    TermLevel.Operation opn ->
      genOperation genT opn

    TermLevel.Abstraction args body ->
      JS.JSFunctionExpression
        JS.JSNoAnnot
        JS.JSIdentNone
        JS.JSNoAnnot
        (genArgs args)
        JS.JSNoAnnot
        (genBody (genT body))

    TermLevel.Application left rights ->
      JS.JSCallExpression
        (genT left)
        JS.JSNoAnnot
        (genCommaList (map genT rights))
        JS.JSNoAnnot

    TermLevel.TypeAbstraction _ _ ->
      error "not implemented yet"

    TermLevel.TypeApplication _ _ ->
      error "not implemented yet"

    TermLevel.RecordIntroduction _ ->
      error "not implemented yet"

    TermLevel.RecordElimination _ _ ->
      error "not implemented yet"


genTermF' :: (t -> JS.JSExpression) -> TermLevel.TermF t -> JS.JSExpression
genTermF' genT termF =
  JS.JSExpressionParen JS.JSNoAnnot (genTermF genT termF) JS.JSNoAnnot


genMuTermF :: Mu TermLevel.TermF -> JS.JSExpression
genMuTermF (Fix termF) = genTermF' genMuTermF termF


genTerm :: TermLevel.Term -> JS.JSExpression
genTerm (TermLevel.Term mu) = genMuTermF mu


renderTerm :: TermLevel.Term -> String
renderTerm term = JS.renderToString (JS.JSAstExpression (genTerm term) JS.JSNoAnnot)
