{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.ShowToken
import qualified Text.Megaparsec.Lexer as L

-- * Helpers

lexeme = L.lexeme space
symbol = L.symbol space

-- * Individual Components

typeName :: Parser String
typeName = lexeme $ do
  first <- upperChar
  rest <- many alphaNumChar
  return (first : rest)

keyword :: String -> Parser ()
keyword kw = lexeme $ void $ string kw

identifier :: Parser String
identifier = lexeme $ do
  first <- lowerChar
  rest <- many alphaNumChar
  return (first : rest)

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

colon :: Parser ()
colon = void (symbol ":")

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = p `sepBy` symbol ","
