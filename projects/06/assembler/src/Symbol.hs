module Symbol where

import Numeric
import Data.Char
import qualified Data.Map.Lazy as Map
import HackParser
import Control.Monad.State

type Binary = String

decToBin :: Int -> Int -> Binary
decToBin x offset = reverse $ map intToDigit $ decToBin' (x + offset)
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

-- assumes argument already in binary, maybe not as many
-- digits as needed, pad fixes this

pad :: Int -> Binary -> Binary
pad n x
  | (length x == n) = x
  | otherwise = pad n ('0':x)

-- only pace where pad will be called
toBinAddress :: Int -> Int -> Binary
toBinAddress x offset = pad 15 (decToBin x offset)

--built in constant addresses to any program
builtIns' :: [(String,Int)]
builtIns' = [("SP",0),("LCL",1),("ARG",2),("THIS",3),("THAT",4),("R0",0),
             ("R1", 1),("R2",2),("R3",3),("R4",4),("R5",5),("R6",6),("R7",7),
             ("R8", 8),("R9",9),("R10",10),("R11",11),("R12",12),("R13",13),
             ("R14",14),("R15",15), ("SCREEN",16384),("KBD",24576)]

--those same builtins in the correct binary form
builtIns :: SymbolTable
builtIns = SymbolTable (Map.fromList $  (map (\(a,b) -> (a, toBinAddress b 0)) builtIns')) (15)


-- note that the machine code is inserted in lines 0-1023
-- the variable symbols start at 1024
data SymbolTable = SymbolTable
                 { mappings    :: Map.Map String Binary
                 , nextAddress :: Int
                 } deriving (Show)

lookupSymbol :: String -> SymbolTable -> Maybe Binary
lookupSymbol sym table = Map.lookup sym $ mappings table

insertSymbol :: String -> SymbolTable -> SymbolTable
insertSymbol sym table = table { mappings=table', nextAddress = nextAddr }
  where
    nextAddr = nextAddress table + 1
    binary = toBinAddress nextAddr 0
    --- need this line right here to be corrected
    table' = Map.insert sym binary $ mappings table

updateTable :: String -> SymbolTable -> SymbolTable
updateTable label table = case (lookupSymbol label table) of
  Nothing -> insertSymbol label table
  _ -> table


insertJumpMark :: String -> Int -> SymbolTable -> SymbolTable
insertJumpMark sym n table = table { mappings=table' }
  where
    binary = toBinAddress n 0
    table' = Map.insert sym binary $ mappings table


changeExtension :: String -> String
changeExtension ext = (changeExtension' ext)  where
  changeExtension' (x:xs)
    | x == '.' = ".hack"
    | otherwise = x:(changeExtension xs)




-- runRegister :: Register -> Address -> (BinaryAddress, AddressManager)
-- renRegister x _  (am, ba)

-- conversts an Address into binary
-- aToBin :: Address -> LabelBinMap -> BinaryAddress
-- aInstToBin a m = case a of
--   AddressNumber n -> toBinString $ read n
--   AddressLabel l -> let x = lookup l m
--                     in case x of
--                       Just b -> b
-- -- need to implement current HashMap -> Int
--                       -- Nothing -> toBinString $ current m
