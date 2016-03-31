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

    it "parses nested record type" $ do
      let res = parse
                  Parser.parseType
                  "foo.fn"
                  "{a: Foo, b: {c: Bar, d: Baz}}"
      res `shouldBe`
        Right
          (Syntax.RecordType
            (Map.fromList
              [ ("a", Syntax.TypeReference "Foo")
              , ("b", Syntax.RecordType
                  (Map.fromList
                    [ ("c", Syntax.TypeReference "Bar")
                    , ("d", Syntax.TypeReference "Baz")
                    ]
                  )
                )
              ]
            )
          )

    it "parses nested tuple type" $ do
      let res = parse
                  Parser.parseType
                  "foo.fn"
                  "(Foo, Bar, (Baz, Quux))"
      res `shouldBe`
        Right
          (Syntax.TupleType
            [ Syntax.TypeReference "Foo"
            , Syntax.TypeReference "Bar"
            , Syntax.TupleType
              [ Syntax.TypeReference "Baz"
              , Syntax.TypeReference "Quux"
              ]
            ]
          )
