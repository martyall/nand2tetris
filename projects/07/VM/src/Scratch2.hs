module Scratch2 where


import Text.Parsec
import Text.Parsec.Prim
import Parser
import Text.Parsec.Pos
import Data.Functor
import Control.Monad.State.Lazy

-- we use a collection of symbol tables to model the state of memory

data Memory = Memory { fileName :: String
                     , nextBoolIndex :: Int
                     , nextHeapAddress :: Int
                     , staticBase :: Int
                     , staticOffset :: Int
                     , nextReturnAddress :: Int
                     } deriving (Show)

incBoolIndex :: Memory -> Memory
incBoolIndex m = m { nextBoolIndex = (nextBoolIndex m) + 1 }

type Commands = [Command]

type CommandParser = Parsec Commands Memory String

type CommandsParser = Parsec Commands Memory [String]

type PushParser = Parsec Commands Memory Segment

type OpParser = Parsec Commands Memory Op

type CommentParser = Parsec Commands Memory String

type LabelParser = Parsec Commands Memory String

type GotoParser = Parsec Commands Memory String

type IfParser = Parsec Commands Memory String

type CallParser = Parsec Commands Memory Function

type FunctionParser = Parsec Commands Memory Function

type ReturnParser = Parsec Commands Memory String

--
incSP :: CommandParser
incSP =  return "@SP\nM=M+1\n\n"

ram = Memory "foo" 0 2048 16 0 0

--
writePush :: CommandParser
writePush = do
  seg <- grabPushSegment
  content <- getPushContent seg
  x <- return "@SP\nA=M\nM=D\n\n"
  inc <- incSP
  return (content ++ x ++ inc)

getPushContent :: Segment -> CommandParser
getPushContent seg = case seg of
  Constant n -> return ("@" ++ (show n) ++ "\nD=A\n")
  Temp n -> return ("@" ++ (show n) ++ "\nD=A\n@5\nA=D+A\nD=M\n")
  Pointer n -> case n of
    0 -> return "@THIS\nD=M\n"
    1 -> return "@THAT\nD=M\n"
  Static n -> do
    sBase <- staticBase <$> getState
    modifyState (\m -> m { staticOffset = (staticOffset m) + 1 })
    return $ "@" ++ (show n) ++ "\nD=A\n@" ++ (show sBase) ++
             "\nA=D+A\nD=M\n"
  _ -> return ("@" ++ (show $ index seg) ++ "\nD=A\n@" ++ (show seg)
              ++ "\nA=M+D\nD=M\n")

grabPushSegment :: PushParser
grabPushSegment = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_Push seg -> Just seg
    _ -> Nothing

--
grabOp :: OpParser
grabOp = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_Arithmetic op -> Just op
    _ -> Nothing

parseUnary :: Op -> CommandParser
parseUnary (Unary u) = do
  x <- return ("@SP\nAM=M-1\nM=" ++ (show u) ++ "M\n")
  inc <- incSP
  return (x ++ inc)

parseOpArith :: Op -> CommandParser
parseOpArith (Binary (A a)) = do
  x <- return ("@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nM=M" ++ (show a) ++  "D\n")
  inc <- incSP
  return (x ++ inc)

--increasing the stack pointer must be built in here, no need to do it explicitly
parseOpBool :: Op -> CommandParser
parseOpBool (Binary (B b)) = do
  i <- nextBoolIndex <$> getState
  x <- return ("@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nD=M-D\n@true" ++ (show i) ++
               "\nD;" ++ (show b) ++"\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@false" ++ (show i) ++
               "\n0;JMP\n(true" ++ (show i) ++ ")\n@SP\nA=M\nM=-1\n@SP\nM=M+1\n(false" ++
               (show i) ++ ")\n\n")
  modifyState incBoolIndex
  return x

parseOp1 :: Op -> CommandParser
parseOp1 o@(Unary u) = parseUnary o
parseOp1 o@(Binary (A a)) = parseOpArith o
parseOp1 o@(Binary (B b)) = parseOpBool o

writeArithmetic :: CommandParser
writeArithmetic = do
  op <- grabOp
  x <- (parseOp1 op)
  return x

--
writePop :: CommandParser
writePop = do
  x <- return "@SP\nAM=M-1\nD=M\n@R13\nM=D\n"
  seg <- grabPopSegment
  content <- putPopContent seg
  return (x ++ content)

grabPopSegment :: PushParser
grabPopSegment = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_Pop seg -> Just seg
    _ -> Nothing

putPopContent :: Segment -> CommandParser
putPopContent seg = case seg of
  Temp n -> return ("@5\nD=A\n@" ++ (show n) ++
             "\nD=A+D\n@R14\nM=D\n@R13\nD=M\n@R14\nA=M\nM=D\n")
  Pointer n -> case n of
    0 -> return ("@THIS\nM=D\n")
    1 -> return ("@THAT\nM=D\n")
  Static n -> do
         sBase <- staticBase <$> getState
         return ("@" ++ (show sBase) ++"\nD=A\n@" ++ (show n)
               ++ "\nD=A+D\n@R14\nM=D\n@R13\nD=M\n@R14\nA=M\nM=D\n")
  _ -> return ("@" ++ (show $ index seg) ++  "\nD=A\n@" ++ (show seg) ++
               "\nD=M+D\n@R14\nM=D\n@R13\nD=M\n@R14\nA=M\nM=D\n")
--
grabComment :: CommentParser
grabComment = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    Comment comment -> Just comment
    _ -> Nothing

writeComment :: CommentParser
writeComment = do
  comment <- grabComment
  return $ "//" ++ comment ++ "\n"

--

grabLabel :: LabelParser
grabLabel = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_Label label -> Just label
    _ -> Nothing

writeLabel :: CommandParser
writeLabel = do
  label <- grabLabel
  filename <- fileName <$> getState
  return ("(" ++ filename ++ "." ++ label ++ ")")
--


grabGoto :: GotoParser
grabGoto =  token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_Goto label -> Just label
    _ -> Nothing

writeGoto :: CommandParser
writeGoto = do
  label <- grabGoto
  filename <- fileName <$> getState
  return ("@" ++ filename ++ "." ++ label ++"\n0;JMP\n")

--

grabIf :: IfParser
grabIf = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_If iflabel -> Just iflabel
    _ -> Nothing

writeIf :: CommandParser
writeIf = do
  label <- grabIf
  fileName <- fileName <$> getState
  return ("@SP\nAM=M-1\nD=M\n@" ++ fileName ++ "." ++ label ++ "\nD;JNE\n")

--functions

grabFunction :: FunctionParser
grabFunction = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_Function (Function name nargs) -> Just (Function name nargs)
    _ -> Nothing

writeFunction :: CommandParser
writeFunction = do
  filename <- fileName <$> getState
  funInfo <- grabFunction
  --removed filename
  marker <- return $ "(" ++ (name funInfo) ++ ")\n"
  clearLocals <- return (concat $ replicate (nargs funInfo) "@SP\nA=M\nM=0\n@SP\nM=M+1\n")
  return $ marker ++ clearLocals
--

grabCall :: CallParser
grabCall = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_Call (Function name nargs) -> Just (Function name nargs)
    _ -> Nothing

writeCall :: CommandParser
writeCall = do
  filename <- fileName <$> getState
  returnAddr <- ((\s -> "return" ++ s).(show.nextReturnAddress)) <$> getState
  modifyState (\m -> m { nextReturnAddress = (nextReturnAddress m) + 1 })
  pushReturnAddr <- return $ "@" ++ returnAddr ++ "\nD=A\n@SP\nA=M\nM=D\n"
  inc <- incSP
  pushRegs <- return pushAllRegs
  callInfo <- grabCall
  setPntrs <- return $ "@SP\nD=M\n@5\nD=D-A\n@" ++ (show $ nargs callInfo)
                  ++ "\nD=D-A\n@ARG\nM=D\n@SP\nD=M\n@LCL\nM=D\n"
  -- removed filename
  doGoto <- return $ "@" ++ (name callInfo) ++ "\nA;JMP\n"
  returnLabel <- return $ "(" ++ returnAddr ++ ")\n"
  return $ pushReturnAddr ++ inc ++ pushRegs ++ setPntrs ++ doGoto ++ returnLabel


pushRegBase :: Segment -> String
pushRegBase seg = "@" ++ (show seg) ++ "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"

pushAllRegs :: String
pushAllRegs = foldl (++) "" (map pushRegBase [LCL 0, ARG 0, THIS 0, THAT 0])


--
grabReturn :: ReturnParser
grabReturn = token prnt pos mtch where
  prnt = show
  pos = (\_ -> newPos "undefined" 0 0)
  mtch x = case x of
    C_Return "return" -> Just ("return")
    _ -> Nothing

writeReturn :: CommandParser
writeReturn = do
  grabReturn
  let frame = "@LCL\nD=M\n@R13\nM=D\n"
      ret = "@5\nD=A\n@R13\nA=M-D\nD=M\n@R14\nM=D\n"
      reposRetVal = "@SP\nAM=M-1\nD=M\n@ARG\nA=M\nM=D\n"
      restSP = "@ARG\nD=M+1\n@SP\nM=D\n"
      restThat = "@R13\nAM=M-1\nD=M\n@THAT\nM=D\n"
      restThis = "@R13\nAM=M-1\nD=M\n@THIS\nM=D\n"
      restArg = "@R13\nAM=M-1\nD=M\n@ARG\nM=D\n"
      restLcl = "@R13\nAM=M-1\nD=M\n@LCL\nM=D\n"
      gotoRet = "@R14\nA=M\n0;JMP\n"
  return $ frame ++ ret ++ reposRetVal ++ restSP ++ restThat ++ restThis
           ++ restArg ++ restLcl ++ gotoRet



--
writeCommand :: CommandsParser
writeCommand = many $ choice [writeComment, writePop, writePush
                             , writeArithmetic, writeLabel, writeGoto
                             , writeIf, writeFunction, writeCall, writeReturn]

type ScriptParser = Parsec Commands Memory ([String], Memory)

scriptParser :: Parsec Commands Memory ([String], Memory)
scriptParser = do
  output <- writeCommand
  memory <- getState
  return (output, memory)
