{-# LANGUAGE OverloadedStrings #-}

module Monkey.Token
  ( lookupIdent
  ) where

-- External imports
import Data.Text (Text)

-- Internal imports
import Monkey.Types.Token

lookupIdent :: Text -> Token
lookupIdent ident = case ident of
  "fn"     -> FN
  "let"    -> LET
  "true"   -> TRUE
  "false"  -> FALSE
  "if"     -> IF
  "else"   -> ELSE
  "return" -> RETURN
  _        -> IDENT ident
