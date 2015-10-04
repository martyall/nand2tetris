module JackCodeGenerator.CodeGen where

import qualified JackParser.AST as AST
import qualified JackParser.Lexer as L
import JackParser.SymbolTable

import Text.Parsec.Prim
import JackParser.Parser

import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Map as M


push :: String -> String
push s = "push " ++ s ++ "\n"

variable :: Var -> String
variable v = (show $ varScope v) ++ " " ++ (show $ varIndex v)

callFun :: ID -> Int -> String
callFun fn n =  "call " ++ fn ++ " " ++ (show n) ++ "\n"





writeExpr :: AST.EXPR -> StateT SymbolTable Maybe String
writeExpr (AST.BoolConst b) = return $ push $ "constant " ++ (show b)
writeExpr (AST.IntConst n) = return $ push $ "constant " ++ (show n)
writeExpr (AST.Var x) = do
  y <- (getV x) <$> get
  lift $ (push . variable) <$> y
writeExpr (AST.UExpr op ex) = do
  ex <- (writeExpr ex)
  return $ ex ++  (show op) ++ "\n"
writeExpr (AST.BExpr op ex1 ex2) = do
  a <- writeExpr ex1
  b <- writeExpr ex2
  return $ a ++ b ++ (show op) ++ "\n"
writeExpr (AST.App fn args) = do
  as <- putArgs
  fn' <- writeFunName fn
  return $ as ++ callFun fn' n
  where
    putArgs = concat <$> (mapM  writeExpr args)
    n = length args
writeExpr (AST.Access var index) = do
  base <- writeExpr var
  off <- writeExpr index
  rest <- return "add\npop pointer 1\npush that 0\n"
  return (base ++ off ++ rest)

writeFunName :: ID -> StateT SymbolTable Maybe String
writeFunName fn =
  if '.' `elem` fn then return fn else
    do
      t <- get
      return $ (name t) ++ "." ++ fn

------

pop :: String -> String
pop s = "pop " ++ s ++ "\n"

function :: ID -> ID -> Int -> String
function cn nm n = "function " ++ (cn ++ "." ++ nm) ++ " " ++ (show n) ++ "\n"

--- statement blocks
writeStmts :: [AST.AST] -> StateT SymbolTable Maybe String
writeStmts xs = concat <$> mapM writeStmt xs

-- single statements
writeStmt :: AST.AST -> StateT SymbolTable Maybe String

-- class statement
writeStmt (AST.CLASS nm bdy) = do
 modify (\t -> (putName nm t))
 writeStmts bdy

writeStmt (AST.VAR sc ty nm) = do
  modify (\t ->
    case nm of
      [] -> t
      x:xs -> let i = getIndex sc t
              in builder (putV x (Var sc i) t) (AST.VAR sc ty xs))
  return ""

-- assignment statement
writeStmt (AST.LET loc val) =
   case loc of
     (AST.Var nm ) -> do
       x <- writeExpr val
       y <- (getV nm) <$> get
       z <- lift $ (pop . variable) <$> y
       return $ x ++ z
     (AST.Access nm index) -> do
       x <- writeExpr nm
       y <- writeExpr index
       p <- return "add\n"
       s <- return $ pop "pointer 1"
       z <- writeExpr val
       t <- return $ pop "that 0\n"
       return $ concat [x,y,p,s,z,t]

-- function decleration

writeStmt (AST.FUN nm ft args rt bdy) = do
  t <- get
  let f = Fun ft rt (M.fromList args)
  put (putF nm f t)
  dec <- (name <$> get) >>= \cn -> return $ function cn nm (length args)
  pushArgs <- writeStmts $  map (\(a,b) -> (AST.VAR L.Argument b [a])) args
  b <- writeStmts bdy
  return $ dec ++ pushArgs ++ b

writeStmt (AST.RET expr) =
  case expr of
    AST.Empty -> modify clearLocals >> return "return\n"
    _ -> writeExpr expr >>= \s -> modify clearLocals >> (return $ s ++ "return\n")



writeStmt (AST.WHILE cond bdy) = do
  index <- getWhile <$> get
  modify incIf
  lb <- return $ "label WHILE_EXP" ++ (show index) ++ "\n"
  s <- writeExpr cond
  cse <- return $ "if-goto WHILE_END" ++ (show index) ++ "\n"
  b <- writeStmts bdy
  e <- return $ "WHILE_END" ++ (show index) ++ "\n"
  return $ concat [lb,s,cse, b, e]

writeStmt (AST.BRANCH cond iff thn) = do
  index <- getWhile <$> get
  modify incIf
  s <- writeExpr cond
  cse <- return $ "if-goto IF_BODY" ++ (show index) ++ "\n"
  els <- return $ "goto ELSE_BODY" ++ (show index) ++ "\n"
  ibl <- return $ "label IF_BODY" ++ (show index) ++ "\n"
  ib <- writeStmts iff
  ebl <- return $ "label ELSE_BODY" ++ (show index) ++ "\n"
  case thn of
    (Just x) -> writeStmts x >>= \eb ->
      return $ concat [s,cse,els,ibl,ib,ebl,eb]
    Nothing -> return $ concat [s,cse,els,ibl,ib,ebl]

writeStmt (AST.DO fun args) = writeExpr (AST.App fun args)
