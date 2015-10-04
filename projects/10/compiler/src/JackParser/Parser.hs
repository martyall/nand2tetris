module JackParser.Parser where

import JackParser.Lexer as L
import JackParser.AST
import qualified JackParser.Operators as Op

import Control.Monad
import Control.Monad.Reader
import Data.Functor((<$>))

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Prim as PR

----------------------------------------------------------

--- arithmetic and boolean expressions

symbols =  P.choice [ IntConst <$> L.integer
                    , BoolConst <$> L.boolean
                    , StrConst <$> stringLiteral
                    , Var <$> identifier
                    ]

bins = P.choice [L.BB <$> L.bBOp, L.RB <$> L.rBOp, L.BA <$> L.bAOp]

uns = P.choice [L.UB <$> L.uBOp, L.UA <$> L.uAOp]

algebra = Op.Algebra bins uns symbols


expression :: Parser EXPR
expression = runReaderT Op.expression algebra

-- -----------------------------------------------------------
-- -- declerations

-- -- function declaration

funDec :: Parser AST
funDec = do
  ft <- L.funType
  rt <- L.ttypes
  nm <- identifier
  args <- parens $ P.sepBy getDeclarationArg comma
  body <- braces fBody
  return $ FUN nm ft args rt body
  where
    getDeclarationArg = do
      ty <- L.ttypes
      nm <- identifier
      return $ (nm, ty)


-- -- return statement
returnSt :: Parser AST
returnSt = do
  reserved "return"
  rval <- PR.try (look >> return Empty) PR.<|> expression
  semi
  return $ RET rval
  where
    look = PR.lookAhead semi

-- function Body
fBody :: Parser [AST]
fBody = do
  sts <- PR.many statement
  rtr <- returnSt
  return $ sts ++ [rtr]

-- -- function call

call :: Parser AST
call = do
  reserved "do"
  nm <- identifier
  args <- parens $  P.sepBy expression comma
  semi
  return $ DO nm args

-- -- variable decleration

-- probably could use GADL. here for whether there is just one
-- or many vars being declared
varDec :: Parser AST
varDec = do
  sc <- L.scope
  ty <- L.ttypes
  names <- P.sepBy identifier comma
  semi
  return $ VAR sc ty names

varAssign :: Parser AST
varAssign = do
  reserved "let"
  location <- PR.try access PR.<|> (Var <$> identifier)
  reservedOp "="
  expr <- expression
  semi
  return $ LET location expr

access :: Parser EXPR
access = do
  nm <- Var <$> identifier
  e <- brackets expression
  return $ Access nm e

-- -----------------------------------------------------------
-- -- If L.en Else

ifSt :: Parser AST
ifSt = do
  reserved "if"
  cnd <- parens expression
  thenBranch <- braces ifBlock
  elseBranch <- ((PR.try $ Just <$> elseSt) PR.<|> (return Nothing))
  return $ BRANCH cnd thenBranch elseBranch

ifBlock :: Parser [AST]
ifBlock = PR.many statement

elseSt :: Parser [AST]
elseSt = do
  reserved "else"
  b <- braces ifBlock
  return b

-- -- while statement

whileSt :: Parser AST
whileSt = do
  reserved "while"
  cnd <- parens expression
  sts <- braces $ PR.many statement
  return $ WHILE cnd sts


-- -----------------------------------------------------------
-- -- statement

-- comments
comment :: Parser ()
comment =  L.cmt

statement :: Parser AST
statement = P.choice [varDec, varAssign, call, ifSt, whileSt]

classStatement :: Parser AST
classStatement = (PR.many comment) >> ((PR.try statement) PR.<|> funDec)

-- ------------------------------------------------------------
-- class definitions

classDef :: Parser AST
classDef = do
  reserved "class"
  nm <- identifier
  sts <- braces $ PR.many classStatement
  return $ CLASS nm sts

--------------------------------------------------------------

program :: Parser AST
program = (PR.many comment) >> classDef
