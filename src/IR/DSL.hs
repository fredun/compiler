module IR.DSL (termDSL, typeDSL) where

import Data.Data (Data)

import qualified Text.Trifecta as Trifecta

import Language.Haskell.TH.Quote

import qualified IR.Parser as Parser


termDSL :: QuasiQuoter
termDSL = mkQQ Parser.term


typeDSL :: QuasiQuoter
typeDSL = mkQQ Parser.parseType


mkQQ :: Data a => Trifecta.Parser a -> QuasiQuoter
mkQQ parser =
  QuasiQuoter
    { quoteExp = \s -> do
        t <- runParser parser (filter (/= '\r') s)
        dataToExpQ (const Nothing) t
    , quotePat = error "Cannot use dsl as a pattern"
    , quoteType = error "Cannot use dsl as a type"
    , quoteDec = error "Cannot use dsl as a dec"
    }


runParser :: Monad m => Trifecta.Parser a -> String -> m a
runParser parser s =
  case Trifecta.parseString (Trifecta.whiteSpace >> parser) mempty s of
    Trifecta.Failure err -> fail (show err)
    Trifecta.Success res -> return res
