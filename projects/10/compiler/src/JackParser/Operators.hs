module JackParser.Operators

  where

import qualified JackParser.Lexer as L
import JackParser.AST

import Text.Parsec.String (Parser)
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Combinator as PC
import Text.Parsec.Char(char)

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.Functor.Identity
import Control.Monad.Morph


type BinaryOp = L.BOp

type UnaryOp = L.UOp

type Symbol = EXPR

-----------------------

data Algebra = Algebra { bOps :: Parser BinaryOp
                       , uOps :: Parser UnaryOp
                       , symbols :: Parser Symbol
                       }

type AParser = ReaderT Algebra Parser

type ExprTree = AParser EXPR

-- lifted combinators

try :: AParser a -> AParser a
try p = ReaderT $ \r -> P.try (runReaderT p r)

(<|>) :: AParser a -> AParser a -> AParser a
(<|>) p q = ReaderT $ \r ->
  ((runReaderT p r) P.<|> (runReaderT q r))

parens :: AParser a -> AParser a
parens p = ReaderT $ \r -> L.parens (runReaderT p r)

sepBy :: AParser a -> AParser b -> AParser [a]
sepBy p d = (try $ sepBy1 p d)
         <|> (try $ p >>= \r -> return [r])
         <|> return []

sepBy1 :: AParser a -> AParser b -> AParser [a]
sepBy1 p d = p >>= \r -> d >> sepBy p d >>= \r' -> return (r:r')

between :: AParser a -> AParser b -> AParser c -> AParser a
between p o c = ReaderT $ \r ->
  (runReaderT o r) >> (runReaderT p r) >>= \x -> (runReaderT c r) >> (return x)

brackets :: AParser a -> AParser a
brackets p = ReaderT $ \r -> L.brackets (runReaderT p r)

-------------------------------------------------
-- expression parsers

expression :: ExprTree
expression = (try binExpression) <|> (try unExpression) <|> atom


binExpression :: ExprTree
binExpression = atom <##> expression

unExpression :: ExprTree
unExpression = (<#>) expression

atom :: ExprTree
atom = (try funCall) <|> (try access) <|> (try $ parens expression) <|> sym
  where
    sym =  do
      a <- ask
      sym <- lift $ symbols a
      return sym

funCall :: ExprTree
funCall = do
  nm <- lift L.identifier
  args <- parens $ sepBy expression (lift L.comma)
  return $  App nm args

access :: ExprTree
access = do
  nm <- lift L.identifier
  e <- brackets expression
  return $ Access (Var nm) e

(<#>) :: ExprTree -> ExprTree
(<#>) arg = do
  a <- ask
  op <- lift $ uOps a
  opnd <- arg
  return $ UExpr op opnd

(<##>) :: ExprTree -> ExprTree -> ExprTree
(<##>) arg1 arg2 = do
  a1 <- arg1
  a <- ask
  op <-  lift $ bOps a
  a2 <- arg2
  return $ BExpr op a1 a2

