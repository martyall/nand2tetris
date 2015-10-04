module JackParser.Lexer where

--import Data.Aeson
--import GHC.Generics

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok

--the basic language defintion of Jack
languageDef =
  emptyDef  { Tok.commentStart = "/**"
            , Tok.commentEnd = "*/"
            , Tok.commentLine = "//"
            , Tok.identStart = letter <|> char '_'
            , Tok.identLetter = alphaNum <|> oneOf "._"
            , Tok.reservedNames = [ "if", "then", "else", "let", "do", "return", "Return"
                                  , "class", "constructor", "method", "function"
                                  , "var", "static", "field"
                                  , "true", "false", "null"
                                  , "char", "int", "boolean", "void"
                                  ]
            , Tok.reservedOpNames = [ "+", "-", "*", "/", "&"
                                    , "|", "~", "<", ">", "="
                                    ]
            , Tok.caseSensitive = True
            }

lexer = Tok.makeTokenParser languageDef


--identifiers
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

--groupers
parens = Tok.parens lexer
braces = Tok.braces lexer
brackets = Tok.brackets lexer

-- literals
stringLiteral = Tok.stringLiteral lexer

-- seperators
semi = Tok.semi lexer
comma = Tok.comma lexer
commaSep = Tok.commaSep lexer
whiteSpace = Tok.whiteSpace lexer
lexeme = Tok.lexeme lexer


------------------------------------------------------------

-- less primitive lexers

-----------------------------------------------------
-- Functions

type Identifier = String

data FunType = Function
             | Method
             | Constructor
             deriving (Show, Eq)

funType :: Parser FunType
funType = choice [ reserved "constructor" >> return Constructor
                 , reserved "function" >> return Function
                 , reserved "method" >> return Method
                 ]

type TType = String

ttypes :: Parser TType
ttypes = choice [ reserved "void" >> (return  "void")
                , reserved "int" >> (return  "int")
                , reserved "boolean" >> (return "boolean")
                , reserved "char" >> (return  "char")
                , identifier >>= \t -> (return t)
                ]

------------------------------------------------------
-- Variables

data Scope = Field | Static | Local | Argument deriving (Eq)

instance Show Scope where
  show Field = "this"
  show Static = "static"
  show Local = "local"
  show Argument = "argument"

scope :: Parser Scope
scope = choice [ reserved "field" >> return Field
               , reserved "static" >> return Static
               , reserved "var" >> return Local
               ]

type VarType = String
----------------------------------------------------------
-- Expressions

-- expression Tokens


data BOp = BA BAOp
         | BB BBOp
         | RB RBOp
         deriving Eq

instance Show BOp where
  show (BA x) = show x
  show (BB x) = show x
  show (RB x) = show x


data UOp = UA UAOp
         | UB UBOp
         deriving (Eq)

instance Show UOp where
  show (UA x) = show x
  show (UB x) = show x

--binary boolean operator

data BBOp = And | Or deriving (Eq)

instance Show BBOp where
  show And = "and"
  show Or = "or"

bBOp :: Parser BBOp
bBOp = choice [ reserved "&" >> return And
              , reserved "|" >> return Or
              ]

--unary boolean operator
data UBOp = Not  deriving (Eq)

instance Show UBOp where
  show Not = "not"

uBOp :: Parser UBOp
uBOp = reservedOp "~" >> return Not

--relational boolean operator
data RBOp = Eq | Lt | Gt deriving (Eq)

instance Show RBOp where
  show Eq = "eq"
  show Lt = "lt"
  show Gt = "gt"

rBOp :: Parser RBOp
rBOp = choice [ reservedOp "=" >> return Eq
              , reservedOp "<" >> return Lt
              , reservedOp ">" >> return Gt
              ]

-------------------------------------------------------
-- arithmetic expressionss

--binary arithemtic operator
data BAOp = Add | Sub | Mult | Div deriving (Eq)


instance Show BAOp where
  show Add = "add"
  show Sub = "sub"
  show Mult = "call Math.multiply 2"
  show Div = "call Math.divide 2"

bAOp :: Parser BAOp
bAOp = choice [ reservedOp "+" >> return Add
              , reservedOp "-" >> return Sub
              , reservedOp "*" >> return Mult
              , reservedOp "/" >> return Div
              ]

--unary arithmetic operator
data UAOp = Neg deriving (Eq)

instance Show UAOp where
  show Neg = "neg"

uAOp :: Parser UAOp
uAOp = reservedOp "-" >> return Neg

--------------------------------------------

--literals

-- boolean literals

data Boolean = T | F  deriving (Eq)

instance Show Boolean where
  show T = "-1"
  show F = "0"

boolean :: Parser Boolean
boolean = choice [ reserved "true" >> return T
                 , reserved "false" >> return F
                 ]

--int literal

type IntLit = Int

integer :: Parser IntLit
integer = lexeme $ do
  n <- read <$> (many1 digit)
  return n

--------------------------------------------
--comments

type Comment = ()

lineC :: Parser Comment
lineC = lexeme $ do
  string "//"
  manyTill anyChar (oneOf "\n\r")
  return ()

blockC :: Parser Comment
blockC = lexeme $ do
  string "/**"
  manyTill anyChar (try (string "*/"))
  return $ ()

cmt = (try blockC) <|> lineC
