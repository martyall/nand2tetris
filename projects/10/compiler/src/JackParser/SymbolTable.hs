{-# LANGUAGE TemplateHaskell #-}

module JackParser.SymbolTable where

import qualified Data.Map as M
import qualified JackParser.Lexer as L
import qualified JackParser.AST as AST
import Control.Monad.State.Lazy
import Control.Lens


type ID = L.Identifier

data Var = Var { varScope :: L.Scope
               , varIndex :: Int
               } deriving (Eq, Show)

data LocalVars = LocalVars { _localV :: M.Map ID Var
                           , _lclLIndex :: Int
                           , _argLIndex :: Int
                           } deriving (Eq, Show)
makeLenses ''LocalVars

--type FVarMap = M.Map ID Var

data Fun = Fun { funType :: L.FunType
               , retType :: L.Identifier
               , args :: M.Map L.Identifier L.VarType
               } deriving (Eq, Show)

data ClassVars = ClassVars { _classV :: M.Map ID Var
                           , _functions :: M.Map ID Fun
                           , _staticCIndex :: Int
                           , _fieldCIndex :: Int
                           } deriving (Eq, Show)
makeLenses ''ClassVars


data SymbolTable = SymbolTable { _c :: ClassVars
                               , _l :: LocalVars
                               , _className :: ID
                               , _whileIndex :: Int
                               , _ifIndex :: Int
                               } deriving (Eq, Show)
makeLenses ''SymbolTable

--------------------------------------------------------------
-- deal with variable tables

--indexing
getIndex :: L.Scope -> SymbolTable -> Int
getIndex s = view $ case s of
    L.Static -> c . staticCIndex
    L.Field -> c . fieldCIndex
    L.Local -> l . lclLIndex
    L.Argument -> l . argLIndex

-- getCIndex :: L.Scope -> SymbolTable -> Int
-- getCIndex s = view $ case s of
--   L.Static -> c . staticCIndex
--   L.Field -> c . fieldCIndex

-- getLIndex :: L.Scope -> SymbolTable -> Int
-- getLIndex s = view $ case s of
--   L.Local -> l . lclLIndex
--   L.Argument -> l . argLIndex

inc ::  L.Scope -> SymbolTable -> SymbolTable
inc s = over ln succ
  where
    ln = case s of
      L.Static -> c . staticCIndex
      L.Field -> c . fieldCIndex 
      L.Local -> l . lclLIndex
      L.Argument -> l . argLIndex

-- incC :: L.Scope -> SymbolTable -> SymbolTable
-- incC s = over ln succ
--   where
--     ln = case s of
--       L.Local -> c . lclCIndex
--       L.Static -> c . staticCIndex
--       L.Field -> c . fieldCIndex

-- incL :: L.Scope -> SymbolTable -> SymbolTable
-- incL s = over ln succ
--   where
--     ln = case s of
--       L.Local -> l . lclLIndex
--       L.Argument -> l . argLIndex

------------
--putting and getting

putV :: ID -> Var -> SymbolTable -> SymbolTable
putV nm v@(Var sc _) t
  | (sc == L.Local || sc ==  L.Argument) = over (l . localV) (M.insert nm v) (inc sc t)
  | otherwise = over (c . classV) (M.insert nm v) (inc sc t)

-- putCV :: ID -> Var -> SymbolTable -> SymbolTable
-- putCV nm v@(Var sc _) t = over (c . classV) (M.insert nm v) (inc sc t)

-- putLV :: ID -> Var -> SymbolTable -> SymbolTable
-- putLV nm v@(Var sc _) t = over (l . localV) (M.insert nm v) (inc sc t)


getV :: ID -> SymbolTable -> Maybe Var
getV nm t = case M.lookup nm (view (l . localV) t) of
  x@(Just _) ->  x
  _ ->  M.lookup nm (view (c . classV) t)


--deal with the function table

putF :: ID -> Fun -> SymbolTable -> SymbolTable
putF nm f t = over (c . functions) (M.insert fnm f) t
  where fnm =  (view className t )++ "." ++ nm

getF :: ID -> SymbolTable -> Maybe Fun
getF nm t = let map = view (c . functions) t
            in M.lookup ((view className t) ++ "." ++ nm) map

-- class name and unique identifiers of branch statements
putName :: ID -> SymbolTable -> SymbolTable
putName n = set className n

name :: SymbolTable -> ID
name = view className

incWhile :: SymbolTable -> SymbolTable
incWhile = over whileIndex succ

getWhile :: SymbolTable -> Int
getWhile = view whileIndex

incIf :: SymbolTable -> SymbolTable
incIf = over ifIndex succ

getIf :: SymbolTable -> Int
getIf = view whileIndex



------------------------------------------------------------------------

clearLocals :: SymbolTable -> SymbolTable
clearLocals = set  l (LocalVars M.empty 0 0)

emptyTable :: SymbolTable
emptyTable = SymbolTable  (ClassVars M.empty M.empty 0 0) (LocalVars M.empty 0 0) "" 0 0


-- ----------------------------------------------------------

-- --use a fold over the tree to build a symbol table

blockBuilder :: SymbolTable -> [AST.AST] -> SymbolTable

blockBuilder t xs = foldl builder t xs

----
builder :: SymbolTable -> AST.AST -> SymbolTable

builder t (AST.CLASS n x) = blockBuilder (putName n t) x

builder t (AST.VAR sc ty nm) =
  case nm of
    [] -> t
    x:xs -> let i = getIndex sc t
            in builder (putV x (Var sc i) t) (AST.VAR sc ty xs)

builder t (AST.FUN nm ft args rt bdy) =
  let f = Fun ft rt (M.fromList args)
      t' = (putF nm f t)
  in blockBuilder t' $ map (\(a,b) -> (AST.VAR L.Argument b [a])) args

builder t (AST.RET _) = clearLocals t

builder t (AST.BRANCH cond thenB elseB) = undefined

builder t (AST.WHILE cond bdy ) = undefined

builder t (AST.DO _ _) = t

builder t (AST.LET _ _) = t

builder t (AST.EXPR _) = t
