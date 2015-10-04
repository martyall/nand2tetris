module Main where

import System.Environment
import Text.Parsec.Prim
import Control.Monad.State.Lazy
import Control.Monad.Reader

import JackParser.Lexer
import JackParser.Parser
import JackParser.SymbolTable
import JackParser.AST
import JackCodeGenerator.CodeGen

main :: IO ()
main = do
  file <- head <$> getArgs
  contents <- readFile file
  --let x = parse program "" contents
  --putStrLn $ show x
  let (Right x) = parse program "" contents
  putStrLn $ show $  runStateT (writeStmt x) emptyTable
