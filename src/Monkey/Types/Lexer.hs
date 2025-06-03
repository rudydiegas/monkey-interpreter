module Monkey.Types.Lexer
  ( Lexer(..)
  ) where

-- External imports
import Data.Text (Text)

data Lexer = Lexer
  { code :: Text
  , ind  :: Int
  , ch   :: Char
  } deriving (Eq, Ord, Show)
