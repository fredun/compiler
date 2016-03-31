{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import qualified Data.Map as Map

import Text.Megaparsec

import Test.Hspec

import qualified Parser
import qualified Syntax

spec :: Spec
spec = do

  describe "parseType" $ do

    it "parses nested structural type" $ do
      let res = parse
                  Parser.parseType
                  "foo.fn"
                  "{a: Foo, b: {c: Bar, d: Baz}}"
      res `shouldBe`
        Right
          (Syntax.StructuralType
            (Map.fromList
              [ ("a", Syntax.TypeReference "Foo")
              , ("b", Syntax.StructuralType
                  (Map.fromList
                    [ ("c", Syntax.TypeReference "Bar")
                    , ("d", Syntax.TypeReference "Baz")
                    ]
                  )
                )
              ]
            )
          )
