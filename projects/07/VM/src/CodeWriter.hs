module CodeWriter where

import Parser
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import System.IO
import System.Environment
import Data.Either.Extra

-- we use a collection of symbol tables to model the state of memory

-- type SymbolTable = Map.Map String Int

-- data Memory = Memory { registers :: SymbolTable
--             , heap :: SymbolTable
--             , nextHeapAddr :: Int
--             , nextBoolIndex :: Int
--             }

-- offsetVal :: String -> Int -> SymbolTable -> SymbolTable
-- offsetVal k offset table = let v = Map.lookup k table
--                  in case v of
--                    Nothing -> table
--                    Just v -> Map.insert k (v + offset) table

-- inc, decOne, decTwo :: SymbolTable -> SymbolTable
-- inc = offsetVal "SP" 1
-- decOne = offsetVal "SP" (-1)
-- decTwo = offsetVal "Sp" (-2)

-- virtualRegisters = [("SP",256),("LCL",1),("ARG",2),("pointer",2048),("R0",0),("R1", 1),
--                     ("R2",2),("R3",3),("R4",4),("R5",5),("R6",6),("R7",7),("R8", 8),
--                     ("R9",9),("R10",10),("R11",11),("R12",12),("R13",13),
--                     ("R14",14),("R15",15), ("temp", 5), ("static", 16)]

-- ram = Memory (Map.fromList virtualRegisters) (Map.empty) 2048 0

-- writeLines :: Memory -> [Command] -> [String]
-- writeLines m cs = case cs of
--   [] -> []
--   (x:xs) -> let (s,m') = writeLine m x
--             in s:(writeLines m' xs)

-- writeLine :: Memory -> Command -> (String, Memory)
-- writeLine m c = case c of
--   C_Arithmetic op -> writeArithmetic op m
--   C_Push p -> writePush p m
--   Comment _ -> ("", m)
--   _-> undefined

-- writePush :: Segment -> Memory -> (String, Memory)
-- writePush seg m = case seg of
--   Constant n -> writePushConstant n m
--   ARG n -> writePushSegment "ARG" n m
--   LCL n -> writePushSegment "LCL" n m
--   Static n -> writePushSegment "static" n m
--   Pointer n -> writePushSegment "pointer" n m
--   Temp n -> writePushSegment "temp" n m
--   THIS -> writePushSegment "pointer" 0 m
--   THAT -> writePushSegment "pointer" 1 m

-- writePop :: Segment -> Memory -> (String, Memory)
-- writePop seg m = case seg of
--   ARG n -> writePopSegment "ARG" n m
--   LCL n -> writePopSegment "LCL" n m
--   Static n -> writePopSegment "static" n m
--   Pointer n -> writePopSegment "pointer" n m
--   Temp n -> writePopSegment "temp" n m
--   THIS -> writePopSegment "pointer" 0 m
--   THAT -> writePopSegment "pointer" 1 m

-- writePushConstant :: Int -> Memory -> (String, Memory)
-- writePushConstant n m = let Just sp = Map.lookup "SP" (registers m)
--                             m'= m { registers = inc (registers m) }
--                         in ("@" ++ (show n) ++ "\nD=A\n@" ++ (show sp) ++ "\nM=D\n", m')

-- writePushSegment :: String -> Int -> Memory -> (String, Memory)
-- writePushSegment k n m = let Just sp = Map.lookup "SP" (registers m)
--                              Just segIndex = Map.lookup k (registers m)
--                              m'= m { registers = inc (registers m) }
--                          in ("@" ++ (show (segIndex + n)) ++ "\nD=M\n@" ++ (show sp) ++
--                              "\nM=D\n", m')

-- writePopSegment :: String -> Int -> Memory -> (String, Memory)
-- writePopSegment k n m = let Just sp = Map.lookup "SP" (registers m)
--                             Just segIndex = Map.lookup k (registers m)
--                             m'= m { registers = decOne (registers m) }
--                          in ("@" ++ (show (segIndex + n)) ++ "\nD=M\n@" ++ (show sp) ++
--                              "\nM=D\n", m')

-- writeArithmetic :: Op -> Memory -> (String, Memory)
-- writeArithmetic op m = case op of
--   Unary u -> writeUnary u m
--   Binary (A a) -> writeArith a m
--   Binary (B b) -> writeBool b m

-- writeUnary :: UOp -> Memory -> (String, Memory)
-- writeUnary u m = let Just sp = Map.lookup "SP" (registers m)
--                      m' = m { registers = decOne (registers m) }
--                  in ("@" ++ (show sp) ++ "\nM=" ++ (show u) ++ "D\n", m')

-- -- i factored this to rely on the fact that the D register is properly set from
-- -- the last push operation
-- writeArith :: ArithOp -> Memory -> (String, Memory)
-- writeArith op m = let Just sp = Map.lookup "SP" (registers m)
--                       m' = m { registers = decOne (registers m) }
--                   in ("@" ++ (show (sp-2)) ++ "\nM=M" ++ (show op) ++ "D\n", m')


-- writeBool :: BoolOp -> Memory -> (String, Memory)
-- writeBool b m = let Just sp = Map.lookup "SP" (registers m)
--                     i = nextBoolIndex m
--                     m' = m { registers = decOne (registers m) , nextBoolIndex = (i + 1)}
--                 in ("@" ++ (show (sp-1)) ++ "\n" ++ "D=M\n@" ++ (show (sp-2)) ++
--                    "\nD=M-D\n" ++  "@true" ++ (show i) ++ "\nD;" ++ (show b)  ++ "\nM=0\n(true" ++
--                    (show i) ++ ")\n@" ++ (show (sp-2)) ++ "\nM=-1\n", m')

-- getRights :: [Either a Command] -> [Command]
-- getRights = rights

-- getCommands :: [String] -> [Command]
-- getCommands xs = getRights (map (regularParse parseCommand) xs)

-- -- main :: IO ()
-- -- main = do
-- --   args <- getArgs
-- --   content <- readFile (args !! 0)
-- --   putStr $ unlines $ writeLines ram (getCommands $ lines content)
