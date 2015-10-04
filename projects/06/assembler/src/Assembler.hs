module Assembler where

import System.IO
import System.Environment
import HackParser
import Numeric
import Data.Char
import Symbol
import Control.Monad.State.Lazy
import Data.Map.Lazy(empty)
import Data.Either.Extra

--get the bits indicating the write
regBits :: Maybe [DataRegister] -> Binary
regBits drs = pad 3 $ decToBin 0 $ case drs of
  Nothing -> 0
  Just ds -> foldl foo 0 ds where
    foo n dr = case dr of
      A -> n + 4
      D -> n + 2
      M -> n + 1

-- get the bits indicating the computation

compBits :: Computation -> Binary
compBits c = case c of
  Op (Const Zero) -> "0101010"
  Op (Const One) -> "0111111"
  Unary Neg (Const One) -> "0111010"
  Op (Reg D) -> "0001100"
  Op (Reg A) -> "0110000"
  Op (Reg M) -> "1110000"
  Unary Not (Reg D) -> "0001101"
  Unary Not (Reg A) -> "0110001"
  Unary Not (Reg M) -> "1110001"
  Unary Neg (Reg D) -> "0001111"
  Unary Neg (Reg A) -> "0110011"
  Unary Neg (Reg M) -> "1110011"
  Binary P (Reg D) (Const One) -> "0011111"
  Binary P (Reg A) (Const One) -> "0110111"
  Binary P (Reg M) (Const One) -> "1110111"
  Binary S (Reg D) (Const One) -> "0001110"
  Binary S (Reg A) (Const One) -> "0110010"
  Binary S (Reg M) (Const One) -> "1110010"
  Binary P (Reg D) (Reg A) -> "0000010"
  Binary P (Reg D) (Reg M) -> "1000010"
  Binary S (Reg D) (Reg A) -> "0010011"
  Binary S (Reg D) (Reg M) -> "1010011"
  Binary S (Reg A) (Reg D) -> "0000111"
  Binary S (Reg M) (Reg D) -> "1000111"
  Binary And (Reg D) (Reg A) -> "0000000"
  Binary And (Reg D) (Reg M) -> "1000000"
  Binary Or (Reg D) (Reg A) -> "0010101"
  Binary Or (Reg D) (Reg M) -> "1010101"


-- get the bits indicating the jump
jumpBits :: Maybe JumpCondition -> Binary
jumpBits js = pad 3 $ decToBin 0 $ case js of
  Nothing -> 0
  Just x -> case x of
    JGT -> 1
    JEQ -> 2
    JGE -> 3
    JLT -> 4
    JNE -> 5
    JLE -> 6
    JMP -> 7

--gets bits for entire c instruction
cInstructionBits :: CInstruction -> Binary
cInstructionBits c = let regs = getWriteRegisters c
                         comp = getComputation c
                         jumps = getJumpCondition c
                     in "111" ++ (compBits comp) ++ (regBits regs) ++ (jumpBits jumps)



-- note that there is NO WAY that the lookup can fail, since it will only be performed after a pass through
-- when all the symbols will be entered into the table
aInstructionBits :: SymbolTable -> AInstruction -> Binary
aInstructionBits table (AInstruction addr) = appendZero $ case addr of
  AddressNumber n -> toBinAddress (read n) 0
  AddressLabel l -> case lookupSymbol l table of
    Just b -> b
  where appendZero b = "0" ++ b

runStateJump :: (Int, [InstLine]) -> (Maybe (String, Int), (Int, [InstLine]))
runStateJump (n, []) = (Nothing, (n,[]))
runStateJump (n, i:is) = case i of
  JumpMarker (AddressLabel l) -> (Just (l, n), (n, is))
  Comment _ -> (Nothing, (n, is))
  _ -> (Nothing, (n+1, is))

jumpMarker :: State (Int, [InstLine]) (Maybe (String, Int))
jumpMarker = state runStateJump

firstPassTable :: [InstLine]  -> SymbolTable
firstPassTable xs = firstPassTable' (0,xs) builtIns where
  firstPassTable' (_,[]) table = table
  firstPassTable' s table = let (a,t) = runState jumpMarker s
                            in case (a,t) of
                            (Just (k,v), t) -> firstPassTable' t (insertJumpMark k v table)
                            (Nothing, t) -> firstPassTable' t table


secondPassTable :: [InstLine] -> SymbolTable -> SymbolTable
secondPassTable [] table = table
secondPassTable (x:xs) table = case x of
  Inst (AInst (AInstruction (AddressLabel label)))->  secondPassTable xs (updateTable label table)
  _ -> secondPassTable xs table

--Inst (AInst (AInstruction (AddressLabel "address")))

getTable :: [InstLine] -> SymbolTable
getTable xs = secondPassTable xs (firstPassTable xs)


assemble :: [InstLine] -> SymbolTable -> [Binary]
assemble xs table = assemble' ys table where
  ys = filter isInstruction xs  -- fix this
  assemble' [] table = []
  assemble' (y:ys) table = case y of
    Inst (AInst a) -> (aInstructionBits table a):(assemble' ys table)
    Inst (CInst c) -> (cInstructionBits c):(assemble' ys table )

isInstruction :: InstLine -> Bool
isInstruction i = case i of
  Inst _ -> True
  _ -> False

takeSuccesses :: [Either a InstLine] -> [InstLine]
takeSuccesses = rights

getInstructions :: [String] -> [InstLine]
getInstructions xs = takeSuccesses ( map (regularParse parseLine) xs)


-- need to implement way to get table, pass it to assemble


main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let xs = getInstructions (lines content)
  writeFile (changeExtension (args !! 0))  (unlines (assemble  xs (getTable xs)))
