{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import qualified Data.Map as Map

import Text.Megaparsec

import Test.Tasty
import Test.Tasty.HUnit

import qualified Frontend.Parser as Parser
import qualified Frontend.Syntax as Syntax

tests :: TestTree
tests = testGroup "Parser"

  [ testGroup "parseType"

    [ testCase "parses nested record type" $ do
        let res = parse
                    Parser.parseType
                    "foo.fn"
                    "{a: Foo, b: {c: Bar, d: Baz}}"
        res @?=
          Right
            ( Syntax.RecordType
              ( Syntax.Record
                ( Map.fromList
                  [ ( "a", Syntax.TypeReference "Foo" )
                  , ( "b", Syntax.RecordType
                      ( Syntax.Record
                        ( Map.fromList
                          [ ( "c", Syntax.TypeReference "Bar" )
                          , ( "d", Syntax.TypeReference "Baz" )
                          ]
                        )
                      )
                    )
                  ]
                )
              )
            )

    , testCase "parses nested tuple type" $ do
        let res = parse
                    Parser.parseType
                    "foo.fn"
                    "(Foo, Bar, (Baz, Quux))"
        res @?=
          Right
            ( Syntax.TupleType
              ( Syntax.Tuple
                [ Syntax.TypeReference "Foo"
                , Syntax.TypeReference "Bar"
                , Syntax.TupleType
                  ( Syntax.Tuple
                    [ Syntax.TypeReference "Baz"
                    , Syntax.TypeReference "Quux"
                    ]
                  )
                ]
              )
            )
    ]

  , testGroup "parseBinding"

    [ testCase "parses type binding to tuple type" $ do
        let res = parse
                    Parser.parseBinding
                    "foo.fn"
                    "type X = (Foo, Bar)"
        res @?=
          Right
            ( Syntax.TypeBinding "X"
              ( Syntax.TupleType
                ( Syntax.Tuple
                  [ Syntax.TypeReference "Foo"
                  , Syntax.TypeReference "Bar"
                  ]
                )
              )
            )

    , testCase "parses type binding to record type" $ do
        let res = parse
                    Parser.parseBinding
                    "foo.fn"
                    "type X = {x: Foo, y: Bar}"
        res @?=
          Right
            ( Syntax.TypeBinding "X"
              ( Syntax.RecordType
                ( Syntax.Record
                  ( Map.fromList
                    [ ("x", Syntax.TypeReference "Foo")
                    , ("y", Syntax.TypeReference "Bar")
                    ]
                  )
                )
              )
            )
    ]

  ]
