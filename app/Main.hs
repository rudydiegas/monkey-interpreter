module Main (main) where

import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS

import Monkey.Lexer (newLexer, collectTokens)

main :: IO ()
main = do
  contents <- BS.readFile "test.monkey"
  let decoded = decodeUtf8 contents
  let l = newLexer decoded
  let toks = collectTokens l
  mapM_ print toks
