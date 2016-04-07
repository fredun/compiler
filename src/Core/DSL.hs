module Core.DSL (dsl) where

import qualified Text.Trifecta as Trifecta

import Control.Applicative ((<|>))

import Language.Haskell.TH.Quote

import qualified Core.TypeLevel as TypeLevel
import qualified Core.TermLevel as TermLevel


dsl :: QuasiQuoter
dsl =
  QuasiQuoter
    { quoteExp = \s -> do
        t <- parse (filter (/= '\r') s)
        dataToExpQ (const Nothing) t
    , quotePat = error "Cannot use dsl as a pattern"
    , quoteType = error "Cannot use dsl as a type"
    , quoteDec = error "Cannot use dsl as a dec"
    }


parse :: Monad m => String -> m TermLevel.Term
parse s =
  case Trifecta.parseString (Trifecta.whiteSpace >> term) mempty s of
    Trifecta.Failure err -> fail (show err)
    Trifecta.Success res -> return res


boolean :: Trifecta.Parser Bool
boolean =
  True <$ Trifecta.symbol "true"

  <|>

  False <$ Trifecta.symbol "false"


constant :: Trifecta.Parser TermLevel.Constant
constant =
  Trifecta.parens $

    TermLevel.NumericConstant <$>
      (Trifecta.symbol "numeric" *> Trifecta.integerOrScientific)

    <|>

    TermLevel.CharConstant <$>
      (Trifecta.symbol "char" *> Trifecta.charLiteral)

    <|>

    TermLevel.StringConstant <$>
      (Trifecta.symbol "string" *> Trifecta.stringLiteral)

    <|>

    TermLevel.BooleanConstant <$>
      (Trifecta.symbol "boolean" *> boolean)


identifier :: Trifecta.Parser TermLevel.Identifier
identifier =
  TermLevel.Identifier <$> Trifecta.stringLiteral


term :: Trifecta.Parser TermLevel.Term
term =
  Trifecta.parens $ fmap TermLevel.Term $

    TermLevel.Constant
      <$> (Trifecta.symbol "constant" *> constant)

    <|>

    TermLevel.Variable
      <$> (Trifecta.symbol "variable" *> identifier)

    <|>

    TermLevel.Abstraction
      <$> (Trifecta.symbol "abstraction" *> identifier)
      <*> term

    <|>

    TermLevel.Application
      <$> (Trifecta.symbol "application" *> term)
      <*> term
