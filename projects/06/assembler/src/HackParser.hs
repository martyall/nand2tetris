module HackParser  where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char


--general parsing tools
regularParse :: Parser a -> String -> Either ParseError a
regularParse p s = parse p "" s

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t\r"


lexeme :: Parser a -> Parser a
lexeme p = whitespace *> p <* whitespace

data Address = AddressLabel String | AddressNumber String deriving (Show)

parseAddressLabel' :: Parser String
parseAddressLabel' = lexeme $ do
  fc <- firstChar
  rest <- restChars
  return $ (fc:rest)  where
    firstChar = letter <|> (oneOf "_.$:")
    restChars = many1 (letter <|> digit <|> (oneOf "_.$:"))


parseAddressLabel :: Parser Address
parseAddressLabel = AddressLabel <$> (try parseAddressLabel') <|> (AddressNumber <$> many1 (satisfy isDigit))
-- parsers an A-instruction

newtype AInstruction = AInstruction Address deriving (Show)

parseAInst :: Parser AInstruction
parseAInst = lexeme $ AInstruction <$> ((char '@') *> parseAddressLabel)


-- parsers usefulf for C-Instruction
data DataRegister = A | D | M deriving (Show)

parseDataRegister :: Parser DataRegister
parseDataRegister = lexeme $ do
  x <- (oneOf "ADM")
  return $ case () of _
                        | x == 'A' -> A
                        | x == 'D' -> D
                        | x == 'M' -> M

parseWriteRegisters :: Parser [DataRegister]
parseWriteRegisters = lexeme $ do
  x <- many1 parseDataRegister
  void $ char '='
  return x

-- Neg:-, Not:!
data UOp = Neg | Not deriving (Show)

parseUOp :: Parser UOp
parseUOp = lexeme $ do
  x <- (oneOf "!-")
  return $ case () of _
                        | x == '!' -> Not
                        | x == '-' -> Neg


-- P:+, S:-, 0:|, A:&
data BOp = P | S | Or | And deriving (Show)

parseBOp :: Parser BOp
parseBOp = lexeme $ do
  x <- (oneOf "+-|&")
  return $ case () of _
                        | x == '+' -> P
                        | x == '-' -> S
                        | x == '|' -> Or
                        | x == '&' -> And

data Constant = Zero | One  deriving (Show)

parseConstant :: Parser Constant
parseConstant = lexeme $ do
  x <- (oneOf "01")
  return $ case () of _
                        | x == '0' -> Zero
                        | x == '1' -> One

data Operand = Const Constant | Reg DataRegister deriving (Show)

parseConst :: Parser Operand
parseConst = Const <$> parseConstant

parseReg :: Parser Operand
parseReg = lexeme $ Reg <$> parseDataRegister

parseOperand = parseConst <|> parseReg

data Computation = Op Operand | Unary UOp Operand | Binary BOp Operand Operand deriving (Show)

parseUnary :: Parser Computation
parseUnary = lexeme $ do
  x <- parseUOp
  y <- parseOperand
  return $ Unary x y

parseBinary :: Parser Computation
parseBinary = lexeme $ do
  o1 <- parseOperand
  x <- parseBOp
  o2 <- parseOperand
  return $ Binary x o1 o2

parseComputation :: Parser Computation
parseComputation = try parseUnary <|> try parseBinary <|> Op <$> (try parseOperand) 


--parsers for Jump sequence

data JumpCondition = JMP | JGE | JGT | JEQ | JNE | JLE | JLT deriving (Show)

parseJumpConditionString :: Parser String
parseJumpConditionString = lexeme $ choice (fmap (try.string) ["JGT", "JEQ", "JGE", "JLT", "JNE", "JLE", "JMP"])

parseJumpCondition :: Parser JumpCondition
parseJumpCondition = lexeme $ do
  void $ char ';'
  x <- parseJumpConditionString
  return $ case () of _
                        | x == "JMP" -> JMP
                        | x == "JGE" -> JGE
                        | x == "JGT" -> JGT
                        | x == "JEQ" -> JEQ
                        | x == "JNE" -> JNE
                        | x == "JLT" -> JLT
                        | x == "JLE" -> JLE


-- Cinstruction

data CInstruction = CInstruction { getWriteRegisters :: Maybe [DataRegister]
                                    , getComputation :: Computation
                                    , getJumpCondition :: Maybe JumpCondition } deriving (Show)


parseCInstruction :: Parser CInstruction
parseCInstruction = lexeme $ do
  x <- foo parseWriteRegisters
  y <- parseComputation
  z <- foo parseJumpCondition
  return $ CInstruction x y z where
    foo p = lexeme $ Just <$> try p <|> pure Nothing

--

data Instruction = AInst AInstruction | CInst CInstruction deriving (Show)

data InstLine = Inst Instruction | JumpMarker Address | Comment () deriving (Show)

parseInstruction :: Parser InstLine
parseInstruction = lexeme $ Inst <$> (try (AInst <$> parseAInst) <|> CInst <$> parseCInstruction)
 

parseJumpMarker :: Parser InstLine
parseJumpMarker = lexeme $ JumpMarker <$> ((char '(') *> parseAddressLabel <* (char ')'))


-- This is the comment eater
parseComment :: Parser InstLine
parseComment = Comment <$> do
  void $ string "//"
  void $ many1 (noneOf "\n")
  return ()

parseLine :: Parser InstLine
parseLine = lexeme $ try parseInstruction <|> try parseJumpMarker <|> parseComment

