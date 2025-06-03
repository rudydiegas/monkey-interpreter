module Monkey.REPL
  ( repl
  ) where

-- External imports
import System.IO (hFlush, stdout)
import qualified Data.Text.IO as T

-- Internal imports
import Monkey.Lexer (newLexer, collectTokens)

repl :: IO ()
repl = do
  putStr "🐒> "
  hFlush stdout
  code <- T.getLine
  let l = newLexer code
  let toks = collectTokens l
  mapM_ print toks
  repl
