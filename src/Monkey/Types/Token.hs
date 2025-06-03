module Monkey.Types.Token
  ( Token(..)
  ) where

-- External imports
import Data.Text (Text)

data Token
  -- Error + EOF detection
  = ILLEGAL Char
  | EOF

  -- Delimiters
  | COMMA   -- ","
  | SCOLON  -- ";"
  | LPAREN  -- "("
  | RPAREN  -- ")"
  | LBRACE  -- "{"
  | RBRACE  -- "}"

  -- Assignment operation
  | ASSIGN  -- "="

  -- Arithmetic operations
  | ADD     -- "+"
  | SUB     -- "-"
  | MUL     -- "*"
  | DIV     -- "/"

  -- Logical operations
  | EQ'     -- "=="
  | NEQ     -- "!="
  | LT'     -- "<"
  | GT'     -- ">"
  | BANG    -- "!"

  -- Identifiers and literals
  | IDENT Text
  | INT Int

  -- Keywords
  | FN      -- "function"
  | LET     -- "let"
  | TRUE    -- "true"
  | FALSE   -- "false"
  | IF      -- "if"
  | ELSE    -- "else"
  | RETURN  -- "return"
  deriving (Eq, Ord, Show)
