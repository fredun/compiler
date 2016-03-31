module Lexer where

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.ShowToken
import qualified Text.Megaparsec.Lexer as L

-- * Individual Components

-- ** Symbols

data Symbol =
    OpenParenSymbol
  | CloseParenSymbol
  | OpenCurlySymbol
  | CloseCurlySymbol
  | EqualSymbol
  | CommaSymbol
  | ColonSymbol
  | PlusSymbol
  deriving (Eq, Ord, Show)

parseSymbol :: Parser Symbol
parseSymbol =
      OpenParenSymbol
        <$ string "("

  <|> CloseParenSymbol
        <$ string ")"

  <|> OpenCurlySymbol
        <$ string "{"

  <|> CloseCurlySymbol
        <$ string "}"

  <|> EqualSymbol
        <$ string "="

  <|> CommaSymbol
        <$ string ","

  <|> PlusSymbol
        <$ string "+"

-- ** Keywords

data Keyword =
    TypeKeyword
  | LetKeyword
  | MatchKeyword
  | CaseKeyword
  deriving (Eq, Ord, Show)

parseKeyword :: Parser Keyword
parseKeyword =
      TypeKeyword
        <$ string "type"

  <|> LetKeyword
        <$ string "let"

  <|> MatchKeyword
        <$ string "match"

  <|> CaseKeyword
        <$ string "case"

-- ** Tokens

data Token =
    SymbolToken Symbol
  | KeywordToken Keyword
  deriving (Eq, Ord, Show)

instance ShowToken Token where
  showToken = show

parseToken :: Parser Token
parseToken =
      SymbolToken
        <$> parseSymbol

  <|> KeywordToken
        <$> parseKeyword

-- * Overall Lexer

lexeme :: Parser Token
lexeme = L.lexeme space parseToken

lexer :: Parser [Token]
lexer = many lexeme
