module JackCodeGenerator.Expressions where

import qualified JackParser.AST as AST
import qualified JackParser.Lexer as L
import JackParser.SymbolTable

import Text.Parsec.Prim
import JackParser.Parser

import Control.Monad.Reader

push :: String -> String
push s = "push " ++ s ++ "\n"

variable :: Var -> String
variable v = (show $ _varScope v) ++ " " ++ (show $ _varIndex v)

eval :: L.Identifier -> String
eval fn =  "eval " ++ fn ++ "\n"


writeExpr :: AST.EXPR -> ReaderT SymbolTable Maybe String
writeExpr (AST.BoolConst b) = return $ push $ show b
writeExpr (AST.IntConst n) = return $ push $ show n
writeExpr (AST.Var x) = do
  y <- (getV x) <$> ask
  lift $ (push . variable) <$> y
writeExpr (AST.UExpr op ex) = do
  ex <- (writeExpr ex)
  return $ ex ++ (push $ show op)
writeExpr (AST.BExpr op ex1 ex2) = do
  a <- writeExpr ex1
  b <- writeExpr ex2
  return $ a ++ b ++ (push $ show op)
writeExpr (AST.App fn args) = putArgs >>= \s -> (return $ s ++ eval fn)
  where
    putArgs = concat <$> (mapM  writeExpr args)
writeExpr (AST.Access var index) =
  (writeExpr var) >>= \s -> (writeExpr index) >>= \t -> return (s ++ t ++ "add")

----------------------------------

pop :: String -> String
pop s = "pop " ++ s ++ "\n"

--- statement blocks
writeStmts :: [AST.AST] -> ReaderT SymbolTable Maybe String
writeStmts xs = concat <$> mapM writeStmt xs

-- single statements
writeStmt :: AST.AST -> ReaderT SymbolTable Maybe String

-- class statement
writeStmt (AST.CLASS _ bdy) = writeStmts bdy
writeStmt (AST.VAR _ _ _) = return ""

-- assignment statement
writeStmt (AST.LET loc val) =
   case loc of
     (AST.Var nm ) -> do
       x <- writeExpr val
       y <- (getV nm) <$> ask
       z <- lift $ (pop . variable) <$> y
       return $ x ++ z
     (AST.Access nm index) -> do
       x <- writeExpr nm
       y <- writeExpr index
       p <- return "add\n"
       s <- return $ pop "pointer 0\n"
       z <- writeExpr val
       t <- return $ pop "this 0\n"
       return $ concat [x,y,p,s,z,t]
