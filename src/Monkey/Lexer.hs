{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Monkey.Lexer
  ( newLexer
  , nextToken
  , collectTokens
  ) where

-- External imports
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text (Text, uncons, snoc, unpack)

-- Internal imports
import Monkey.Token (lookupIdent)
import Monkey.Types.Lexer (Lexer(..))
import Monkey.Types.Token (Token(..))

readChar' :: Text -> (Char, Text)
readChar' t = case uncons t of
  Nothing -> ('\0', "")
  Just (cur, rest) -> (cur, rest)

readChar :: Lexer -> Lexer
readChar l = case readChar' l.code of
  ('\0', _) -> l
    { ch = '\0'
    }
  (cur, rest) -> l
    { code = rest
    , ind = l.ind + 1
    , ch = cur
    }

newLexer :: Text -> Lexer
newLexer src = Lexer
  { code = rest
  , ind  = 0
  , ch   = cur
  }
  where
    (cur, rest) = readChar' src

skipWhitespace :: Lexer -> Lexer
skipWhitespace l =
  if isSpace l.ch
  then skipWhitespace $ readChar l
  else l

peekChar :: Lexer -> Char
peekChar l = next
  where
    (next, _) = readChar' l.code

isIdentChar :: Char -> Bool
isIdentChar c = isAlpha c || c == '_'

readIdentifier' :: Text -> Lexer -> (Text, Lexer)
readIdentifier' ident l =
  if isIdentChar l.ch
  then readIdentifier' (snoc ident l.ch) $ readChar l
  else (ident, l)

readIdentifier :: Lexer -> (Text, Lexer)
readIdentifier = readIdentifier' ""

readNumber' :: Text -> Lexer -> (Text, Lexer)
readNumber' dig l =
  if isDigit l.ch
  then readNumber' (snoc dig l.ch) $ readChar l
  else (dig, l)

-- Should only be called when isDigit l.ch, otherwise 0 is returned
readNumber :: Lexer -> (Int, Lexer)
readNumber l = case readNumber' "" l of
  ("", _) -> (0, l)
  (dig, l_new) -> (read $ unpack dig, l_new)

nextToken :: Lexer -> (Token, Lexer)
nextToken l =
  let l_sw = skipWhitespace l
      l_nc = readChar l_sw
  in case l_sw.ch of
    -- Delimiters
    ',' -> (COMMA,  l_nc)
    ';' -> (SCOLON, l_nc)
    '(' -> (LPAREN, l_nc)
    ')' -> (RPAREN, l_nc)
    '{' -> (LBRACE, l_nc)
    '}' -> (RBRACE, l_nc)

    -- Assignment and EQ
    '=' -> if peekChar l_sw == '='
           then (EQ', readChar l_nc)
           else (ASSIGN, l_nc)

    -- Arithmetic ops
    '+' -> (ADD, l_nc)
    '-' -> (SUB, l_nc)
    '*' -> (MUL, l_nc)
    '/' -> (DIV, l_nc)

    -- Logical ops
    '<' -> (LT', l_nc)
    '>' -> (GT', l_nc)
    '!' -> if peekChar l_sw == '='
           then (NEQ, readChar l_nc)
           else (BANG, l_nc)

    '\0' -> (EOF, l_nc)

    -- Multi-character or illegal
    ch'
      | isIdentChar ch' ->
          let (ident, l_id) = readIdentifier l_sw
          in (lookupIdent ident, l_id)
      | isDigit ch' ->
          let (dig, l_dig) = readNumber l_sw
          in (INT dig, l_dig)
      | otherwise -> (ILLEGAL l_sw.ch, l_nc)

-- TODO: improve perf, appending using the ++ operator is an O(N) operation
collectTokens' :: [Token] -> Lexer -> [Token]
collectTokens' toks l = case nextToken l of
  (EOF, _) -> toks ++ [EOF]
  (tok, l_new) -> collectTokens' (toks ++ [tok]) l_new

collectTokens :: Lexer -> [Token]
collectTokens = collectTokens' []
