module IR.Parser where

import qualified Text.Trifecta as Trifecta

import Control.Applicative ((<|>))

import Data.Generics.Fixplate (Mu(..))

import qualified Core.TypeLevel as TypeLevel
import qualified Core.TermLevel as TermLevel


boolean :: Trifecta.Parser Bool
boolean =
  True <$ Trifecta.symbol "true"

  <|>

  False <$ Trifecta.symbol "false"


bitwidth :: Trifecta.Parser TermLevel.BitWidth
bitwidth =
  TermLevel.BitWidth
    <$> Trifecta.integer


numeric :: Trifecta.Parser TermLevel.Numeric
numeric =
  Trifecta.parens $

    TermLevel.NumericFloat
      <$ Trifecta.symbol "float"
      <*> bitwidth
      <*> Trifecta.scientific

    <|>

    TermLevel.NumericSigned
      <$ Trifecta.symbol "integer"
      <*> bitwidth
      <*> Trifecta.integer

    <|>

    TermLevel.NumericUnsigned
      <$ Trifecta.symbol "unsigned"
      <*> bitwidth
      <*> Trifecta.integer

constant :: Trifecta.Parser TermLevel.Constant
constant =
  Trifecta.parens $

    TermLevel.NumericConstant
      <$ Trifecta.symbol "numeric"
      <*> numeric

    <|>

    TermLevel.CharConstant
      <$ Trifecta.symbol "char"
      <*> Trifecta.charLiteral

    <|>

    TermLevel.StringConstant
      <$ Trifecta.symbol "string"
      <*> Trifecta.stringLiteral

    <|>

    TermLevel.BooleanConstant
      <$ Trifecta.symbol "boolean"
      <*> boolean

operator :: Trifecta.Parser TermLevel.Operator
operator =
  TermLevel.Operator <$> Trifecta.stringLiteral

operation :: Trifecta.Parser t -> Trifecta.Parser (TermLevel.Operation t)
operation inner =
  Trifecta.parens $

    TermLevel.BinaryOperation
      <$ Trifecta.symbol "binary"
      <*> operator
      <*> inner
      <*> inner

    <|>

    TermLevel.UnaryOperation
      <$ Trifecta.symbol "unary"
      <*> operator
      <*> inner

identifier :: Trifecta.Parser TermLevel.Identifier
identifier =
  TermLevel.Identifier <$> Trifecta.stringLiteral


typeIdentifier :: Trifecta.Parser TypeLevel.Identifier
typeIdentifier =
  TypeLevel.Identifier <$> Trifecta.stringLiteral


parseType :: Trifecta.Parser TypeLevel.Type
parseType =
  Trifecta.parens $ fmap (TypeLevel.Type . Fix) $

    TypeLevel.Variable
      <$ Trifecta.symbol "type-variable"
      <*> typeIdentifier


termF :: Trifecta.Parser t -> Trifecta.Parser (TermLevel.TermF t)
termF inner =
  Trifecta.parens $

    TermLevel.Constant
      <$ Trifecta.symbol "constant"
      <*> constant

    <|>

    TermLevel.Variable
      <$ Trifecta.symbol "variable"
      <*> identifier

    <|>

    TermLevel.Abstraction
      <$ Trifecta.symbol "abstraction"
      <*> Trifecta.many identifier
      <*> inner

    <|>

    TermLevel.Application
      <$ Trifecta.symbol "application"
      <*> inner
      <*> Trifecta.many inner

    <|>

    TermLevel.TypeAbstraction
      <$ Trifecta.symbol "type-abstraction"
      <*> Trifecta.many typeIdentifier
      <*> inner

    <|>

    TermLevel.TypeApplication
      <$ Trifecta.symbol "type-application"
      <*> inner
      <*> Trifecta.many parseType

    <|>

    TermLevel.Operation
      <$ Trifecta.symbol "operation"
      <*> operation inner

muTermF :: Trifecta.Parser (Mu TermLevel.TermF)
muTermF = Fix <$> termF muTermF


term :: Trifecta.Parser TermLevel.Term
term =
  TermLevel.Term <$> muTermF
