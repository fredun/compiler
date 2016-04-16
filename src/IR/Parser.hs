module IR.Parser where

import qualified Text.Trifecta as Trifecta

import Control.Applicative ((<|>))

import Data.Generics.Fixplate (Mu(..))

import qualified Syntax.Type as Type
import qualified Syntax.Term as Term


boolean :: Trifecta.Parser Bool
boolean =
  True <$ Trifecta.symbol "true"

  <|>

  False <$ Trifecta.symbol "false"


bitwidth :: Trifecta.Parser Term.BitWidth
bitwidth =
  Term.BitWidth
    <$> Trifecta.integer


numeric :: Trifecta.Parser Term.Numeric
numeric =
  Trifecta.parens $

    Term.NumericFloat
      <$ Trifecta.symbol "float"
      <*> bitwidth
      <*> Trifecta.scientific

    <|>

    Term.NumericSigned
      <$ Trifecta.symbol "integer"
      <*> bitwidth
      <*> Trifecta.integer

    <|>

    Term.NumericUnsigned
      <$ Trifecta.symbol "unsigned"
      <*> bitwidth
      <*> Trifecta.integer


constant :: Trifecta.Parser Term.Constant
constant =
  Trifecta.parens $

    Term.NumericConstant
      <$ Trifecta.symbol "numeric"
      <*> numeric

    <|>

    Term.CharConstant
      <$ Trifecta.symbol "char"
      <*> Trifecta.charLiteral

    <|>

    Term.StringConstant
      <$ Trifecta.symbol "string"
      <*> Trifecta.stringLiteral

    <|>

    Term.BooleanConstant
      <$ Trifecta.symbol "boolean"
      <*> boolean


operator :: Trifecta.Parser Term.Operator
operator =
  Term.Operator <$> Trifecta.stringLiteral


operation :: Trifecta.Parser t -> Trifecta.Parser (Term.Operation t)
operation inner =
  Trifecta.parens $

    Term.BinaryOperation
      <$ Trifecta.symbol "binary"
      <*> operator
      <*> inner
      <*> inner

    <|>

    Term.UnaryOperation
      <$ Trifecta.symbol "unary"
      <*> operator
      <*> inner


identifier :: Trifecta.Parser Term.Identifier
identifier =
  Term.Identifier <$> Trifecta.stringLiteral


typeIdentifier :: Trifecta.Parser Type.Identifier
typeIdentifier =
  Type.Identifier <$> Trifecta.stringLiteral


parseType :: Trifecta.Parser (Type.Type Type.Identifier)
parseType =
  Trifecta.parens $ fmap Fix $

    Type.Variable
      <$ Trifecta.symbol "type-variable"
      <*> typeIdentifier


kind :: Trifecta.Parser Type.Kind
kind =
  Trifecta.parens $

    Type.KindOfTypeConstructors
      <$ Trifecta.symbol "type-constructor"
      <*> kind
      <*> kind

    <|>

    Type.KindOfTypes
      <$ Trifecta.symbol "type"


typeArgument :: Trifecta.Parser (Type.Identifier, Type.Kind)
typeArgument =
  (,) <$> typeIdentifier <*> kind


termF :: Trifecta.Parser t -> Trifecta.Parser (Term.TermF Type.Identifier Term.Identifier t)
termF inner =
  Trifecta.parens $

    Term.Constant
      <$ Trifecta.symbol "constant"
      <*> constant

    <|>

    Term.Variable
      <$ Trifecta.symbol "variable"
      <*> identifier

    <|>

    Term.Abstraction
      <$ Trifecta.symbol "abstraction"
      <*> Trifecta.many identifier
      <*> inner

    <|>

    Term.Application
      <$ Trifecta.symbol "application"
      <*> inner
      <*> Trifecta.many inner

    <|>

    Term.TypeAbstraction
      <$ Trifecta.symbol "type-abstraction"
      <*> Trifecta.many typeArgument
      <*> inner

    <|>

    Term.TypeApplication
      <$ Trifecta.symbol "type-application"
      <*> inner
      <*> Trifecta.many parseType

    <|>

    Term.Operation
      <$ Trifecta.symbol "operation"
      <*> operation inner


term :: Trifecta.Parser (Term.Term Type.Identifier Term.Identifier)
term =
  Fix <$> termF term
