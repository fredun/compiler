module Parser where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Megaparsec
import Text.Megaparsec.Text

import qualified Syntax
import qualified Lexer

-- * Records

parseRecordMapping :: Parser a -> Parser (String, a)
parseRecordMapping parseA = do
  field <- Lexer.identifier
  _ <- Lexer.colon
  value <- parseA
  return (field, value)

parseRecordMappings :: Parser a -> Parser (Map String a)
parseRecordMappings parseA =
  Map.fromList
    <$> Lexer.commaSeparated (parseRecordMapping parseA)

parseRecord :: Parser a -> Parser (Syntax.Record a)
parseRecord parseA =
  Syntax.Record
    <$> Lexer.curly (parseRecordMappings parseA)

-- * Tuples

parseTuple :: Parser a -> Parser (Syntax.Tuple a)
parseTuple parseA =
  Syntax.Tuple
    <$> Lexer.parens (Lexer.commaSeparated parseA)

-- * Types

parseTypeReference :: Parser String
parseTypeReference = Lexer.typeName

parseRecordType :: Parser (Syntax.Record Syntax.Type)
parseRecordType = parseRecord parseType

parseTupleType :: Parser (Syntax.Tuple Syntax.Type)
parseTupleType = parseTuple parseType

parseType :: Parser Syntax.Type
parseType =
      Syntax.TypeReference
        <$> parseTypeReference

  <|> Syntax.TupleType
        <$> parseTupleType

  <|> Syntax.RecordType
        <$> parseRecordType

-- * Bindings

parseTypeBinding :: Parser Syntax.Binding
parseTypeBinding = do
  _ <- Lexer.keyword "type"
  i <- Lexer.identifier
  _ <- Lexer.symbol "="
  t <- parseType
  return (Syntax.TypeBinding i t)

parseBinding :: Parser Syntax.Binding
parseBinding = parseTypeBinding
