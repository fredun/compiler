module Parser where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Megaparsec
import Text.Megaparsec.Text

import qualified Syntax
import qualified Lexer

-- * Types

parseTypeReference :: Parser Syntax.Type
parseTypeReference =
  Syntax.TypeReference
    <$> Lexer.typeName

parseTypeMapping :: Parser (String, Syntax.Type)
parseTypeMapping = do
  field <- Lexer.identifier
  _ <- Lexer.colon
  value <- parseType
  return (field, value)

parseTypeMappings :: Parser (Map String Syntax.Type)
parseTypeMappings =
  Map.fromList
    <$> Lexer.commaSeparated parseTypeMapping

parseRecordType :: Parser Syntax.Type
parseRecordType =
  Syntax.RecordType
    <$> Lexer.curly parseTypeMappings

parseTupleType :: Parser Syntax.Type
parseTupleType =
  Syntax.TupleType
    <$> Lexer.parens (Lexer.commaSeparated parseType)

parseType :: Parser Syntax.Type
parseType =
      parseTypeReference
  <|> parseTupleType
  <|> parseRecordType
