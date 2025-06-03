{-# LANGUAGE OverloadedStrings #-}

module Monkey.LexerSpec (spec) where

-- External imports
import Data.Text.Encoding (decodeUtf8)
import Test.Hspec
import qualified Data.ByteString as BS

-- Internal imports
import Monkey.Types.Token (Token(..))
import Monkey.Lexer (newLexer, collectTokens)

monkeyFileDir :: String
monkeyFileDir = "test/Monkey_Examples/"

spec :: Spec
spec = do
  it "should return correct tokens" $ do
    contents <- BS.readFile $ monkeyFileDir ++ "lexing_basic.monkey"
    let decoded = decodeUtf8 contents
    let l = newLexer decoded
    let toks = collectTokens l
    toks `shouldBe` simpleToks

  it "should pass edge cases" $ do
    contents <- BS.readFile $ monkeyFileDir ++ "lexing_edge.monkey"
    let decoded = decodeUtf8 contents
    let l = newLexer decoded
    let toks = collectTokens l
    toks `shouldBe` edgeToks

simpleToks :: [Token]
simpleToks =
  [ LET
  , IDENT "five"
  , ASSIGN
  , INT 5
  , SCOLON
  , LET
  , IDENT "ten"
  , ASSIGN
  , INT 10
  , SCOLON
  , LET
  , IDENT "add"
  , ASSIGN
  , FN
  , LPAREN
  , IDENT "x"
  , COMMA
  , IDENT "y"
  , RPAREN
  , LBRACE
  , IDENT "x"
  , ADD
  , IDENT "y"
  , SCOLON
  , RBRACE
  , SCOLON
  , LET
  , IDENT "result"
  , ASSIGN
  , IDENT "add"
  , LPAREN
  , IDENT "five"
  , COMMA
  , IDENT "ten"
  , RPAREN
  , SCOLON
  , BANG
  , SUB
  , DIV
  , MUL
  , INT 5
  , SCOLON
  , INT 5
  , LT'
  , INT 10
  , GT'
  , INT 5
  , SCOLON
  , IF
  , LPAREN
  , INT 5
  , LT'
  , INT 10
  , RPAREN
  , LBRACE
  , RETURN
  , TRUE
  , SCOLON
  , RBRACE
  , ELSE
  , LBRACE
  , RETURN
  , FALSE
  , SCOLON
  , RBRACE
  , INT 10
  , EQ'
  , INT 10
  , SCOLON
  , INT 10
  , NEQ
  , INT 9
  , SCOLON
  , EOF
  ]

edgeToks :: [Token]
edgeToks = init simpleToks ++
  [ ILLEGAL '^'
  , ILLEGAL '#'
  , ILLEGAL '$'
  , ILLEGAL '😔'
  , IDENT "𝜆"
  , EOF
  ]
