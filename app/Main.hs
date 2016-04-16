module Main where

import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import qualified Text.Trifecta as Trifecta

import qualified Data.ByteString as ByteString

import qualified Core.Unique as Unique
import qualified IR.Parser as Parser
import qualified CodeGen.JavaScript as CodeGen


main :: IO ()
main = do
  contents <- ByteString.getContents

  let result = Trifecta.parseByteString Parser.term mempty contents
  case result of

    Trifecta.Failure err ->
      Doc.putDoc (mappend err Doc.linebreak)

    Trifecta.Success term -> do
      uniqueTerm <- Unique.runUniqueIO (Unique.uniqueTerm mempty term)
      putStrLn (CodeGen.renderTerm uniqueTerm)
