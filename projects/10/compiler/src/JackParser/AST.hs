module JackParser.AST where

import qualified JackParser.Lexer as L

--import Data.Aeson
--import GHC.Generics


type BLOCK = [AST]

data  AST = BLOCK { blockBody :: [AST] }
          |  CLASS { className :: L.Identifier
                  ,  classBody :: BLOCK
                  }
          | VAR { varScope :: L.Scope
                , varType :: L.VarType
                , varNames :: [L.Identifier]
                }
          | FUN { funName :: L.Identifier
                , funType :: L.FunType
                , funArgs :: [(L.Identifier, L.TType)]
                , retType :: L.TType
                , funBody :: BLOCK
                }
          | RET { ret :: EXPR }
          | BRANCH { ifCond :: EXPR
                   , thenBranch :: BLOCK
                   , elseBranch :: Maybe BLOCK
                   }
          | WHILE { whileCond :: EXPR
                  , whileBody :: BLOCK
                  }
          | DO { fun :: L.Identifier
               , args :: [EXPR]
               }
          | LET { loc :: EXPR
                , val :: EXPR
                }
          | EXPR { expr :: EXPR }
          deriving (Show, Eq)

data EXPR =  Empty
            | BoolConst L.Boolean
            | IntConst L.IntLit
            | StrConst String
            | Var L.Identifier
            | UExpr L.UOp EXPR
            | BExpr L.BOp EXPR EXPR
            | App L.Identifier [EXPR]
            | Access EXPR EXPR
            deriving (Show, Eq)

