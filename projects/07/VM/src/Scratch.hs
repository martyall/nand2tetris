module Scratch where

import Parser
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import System.IO
import System.Environment
import Data.Either.Extra
import Control.Monad.State.Lazy
import Data.Maybe
import Control.Applicative
import Data.Functor
import Data.Either
import Control.Monad

-- we use a collection of symbol tables to model the state of memory

-- type SymbolTable = Map.Map String Int

-- data Memory = Memory { registers :: SymbolTable
--                      , nextHeapAddr :: Int
--                      , nextBoolIndex :: Int
--                      } deriving (Show)

-- data PStream = PStream { memory :: Memory
--                        , commands :: [Command]
--                        } deriving (Show)

-- data TranslationError = TranslationError deriving (Show)

-- newtype Translator a = Translator {translate :: PStream -> (Either TranslationError a , PStream)}


-- type PCommand = Translator String


-- instance Functor Translator where
--   fmap f t = Translator $ \s ->
--     let (a, u) = translate t s
--     in case a of
--       Left _ -> (Left TranslationError, u)
--       Right a' -> (Right (f a'), u)

-- instance Applicative Translator where
--   pure a = Translator $ \s -> (Right a, s)
--   f <*> t = Translator $ \s ->
--     let (a, u) = translate t s
--         (phi, v) = translate f u
--     in case (a, phi) of
--       (Right a', Right phi') -> (Right (phi' a'), v)
--       _ -> (Left TranslationError, v)


-- instance Alternative Translator where
--   empty = Translator (\s -> (Left TranslationError, s))
--   t1 <|> t2 = Translator $ \s ->
--     let (a1,u1) = translate t1 s
--         (a2,u2) = translate t2 s
--     in case (a1,a2) of
--       (Right _, _) -> (a1,u1)
--       (Left _, Right _) -> (a2,u2)
--       _ -> (Left TranslationError, s)

-- instance Monad Translator where
--   return = pure
--   t >>= f = Translator $ \s ->
--     let (a,u) = translate t s
--     in case a of
--       Right b -> translate (f b) u
--       _ -> (Left TranslationError, s)

-- instance MonadState PStream Translator where
--   -- Translator PStream
--   get = Translator (\s -> (s,s))
--   put t = Translator (\s -> ((),t))
--   state = Translator


-- virtualRegisters = [("pointer",2048), ("static", 16)]

-- ram = Memory (Map.fromList virtualRegisters) 2048 0



-- --pushing constants and segments
-- -- make the convention that you always move the stack pointer at the beginning.
-- -- this makes it easier to do arithmetic

-- incSP :: PCommand
-- incSP = Translator (\s -> (Right "@SP\nM=M+1\n\n", s))

-- constString :: Segment -> String
-- constString (Constant (Just n)) = "@" ++ (show n) ++ "\nD=A\n@SP\nA=M\nM=D\n\n"

-- pushSegString :: Segment -> PCommand
-- pushSegString seg = do
--   content <- getSegmentContent seg
--   x <- pure "@SP\nA=M\nM=D\n\n"
--   inc <- incSP
--   s <- get
--   put $ s {commands = tail $ commands s}
--   return (content ++ x ++ inc)

-- getSegmentContent :: Segment -> PCommand
-- getSegmentContent seg = case (index seg) of
--   Nothing -> pure $ "@" ++ (show seg) ++ "\nA=M+D\nD=M\n"
--   Just n -> pure $ "@" ++ (show n) ++ "\nD=A\n@" ++ (show seg) ++ "\nA=M+D\nD=M\n"


-- processPush :: PCommand
-- processPush = Translator $ \s -> case (head (commands s)) of
--   C_Push (Constant n) -> (Right $ constString (Constant n),  s {commands = tail $ commands s})
--   C_Push seg -> (Right $ pushSegString seg,  s {commands = tail $ commands s})
--   _ -> (Left TranslationError, s)







-- -- arithmetic
-- --can assume address register currently set to SP
-- unaryString :: Op -> String
-- unaryString (Unary u) = "@SP\nAM=M-1\nM=" ++ (show u) ++ "M\n@SP\nM=M+1\n\n"

-- binArithString :: Op -> String
-- binArithString (Binary (A a)) = "@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nM=M" ++ (show a) ++  "D\n@SP\nM=M+1"

-- binBoolString :: Op -> Memory -> (String, Memory)
-- binBoolString (Binary (B b)) m = ("@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nD=M-D\n@true" ++ (show i) ++
--                                   "\nD;" ++ (show b) ++"\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@false" ++ (show i) ++
--                                   "\n0;JMP\n(true" ++ (show i) ++ ")\n@SP\nA=M\nM=-1\n@SP\nM=M+1\n(false" ++
--                                   (show i) ++ ")\n\n", m')
--                                   where
--                                     i = nextBoolIndex $ m
--                                     m' = m { nextBoolIndex = (nextBoolIndex m) + 1 }

-- processUnary :: PCommand
-- processUnary = Translator $ \s -> case (head (commands s)) of
--   C_Arithmetic u@(Unary _) -> (Right $ unaryString u, s { commands = (tail $ commands s)})
--   _ -> (Left TranslationError, s)

-- processBinArith :: PCommand
-- processBinArith = Translator $ \s -> case (head (commands s)) of
--   C_Arithmetic ba@(Binary (A _)) -> (Right $ binArithString ba, s { commands = (tail $ commands s)})
--   _ -> (Left TranslationError, s)

-- processBinBool :: PCommand
-- processBinBool = Translator $ \s -> case (head (commands s)) of
--   C_Arithmetic bb@(Binary (B _)) -> (Right a, p) where
--     m = memory s
--     r = binBoolString bb m
--     a = fst r
--     p = s { memory = (snd r), commands = (tail $ commands s)}
--   _-> (Left TranslationError, s)


-- processArith :: PCommand
-- processArith = processUnary <|> processBinArith <|> processBinBool

-- processComment :: PCommand
-- processComment = Translator $ \s -> case (head $ commands s) of
--   Comment _ -> (Right "", s { commands = (tail $ commands s)})
--   _ -> (Left TranslationError, s)

-- pp :: PCommand
-- pp = do
--  x <- processPush
--  y <- incSP
--  return (x ++ y)



-- processCommand :: PCommand
-- processCommand = pp <|> processArith <|> processComment


-- writeLines :: PStream -> [String]
-- writeLines (PStream _ []) = []
-- writeLines p = let (a,u) = translate processCommand p
--                in (fromRight a):(writeLines u)



-- -- process :: PCommand
-- -- process = processArith <|> processPush <|> processComment <|> processPop
-- --           <|> processComment

-- -- processPush :: Segment -> Memory -> (String, Memory)
-- -- processPush seg m = case seg of
-- --   Constant n -> process PushConstant n m
-- --   ARG n -> writePushSegment "ARG" n m
-- --   LCL n -> writePushSegment "LCL" n m
-- --   Static n -> writePushSegment "static" n m
-- --   Pointer n -> writePushSegment "pointer" n m
-- --   Temp n -> writePushSegment "temp" n m
-- --   THIS -> writePushSegment "pointer" 0 m
-- --   THAT -> writePushSegment "pointer" 1 m

-- -- writePop :: Segment -> Memory -> (String, Memory)
-- -- writePop seg m = case seg of
-- --   ARG n -> writePopSegment "ARG" n m
-- --   LCL n -> writePopSegment "LCL" n m
-- --   Static n -> writePopSegment "static" n m
-- --   Pointer n -> writePopSegment "pointer" n m
-- --   Temp n -> writePopSegment "temp" n m
-- --   THIS -> writePopSegment "pointer" 0 m
-- --   THAT -> writePopSegment "pointer" 1 m

-- -- writePushConstant :: Int -> Memory -> (String, Memory)
-- -- writePushConstant n m = let Just sp = Map.lookup "SP" (registers m)
-- --                             m'= m { registers = inc (registers m) }
-- --                         in ("@" ++ (show n) ++ "\nD=A\n@" ++ (show sp) ++ "\nM=D\n", m')

-- -- writePushSegment :: String -> Int -> Memory -> (String, Memory)
-- -- writePushSegment k n m = let Just sp = Map.lookup "SP" (registers m)
-- --                              Just segIndex = Map.lookup k (registers m)
-- --                              m'= m { registers = inc (registers m) }
-- --                          in ("@" ++ (show (segIndex + n)) ++ "\nD=M\n@" ++ (show sp) ++
-- --                              "\nM=D\n", m')

-- -- processPush :: PCommand
-- -- processPush = case c of
-- --   C_Push seg
    


-- -- writePopSegment :: String -> Int -> Memory -> (String, Memory)
-- -- writePopSegment k n m = let Just sp = Map.lookup "SP" (registers m)
-- --                             Just segIndex = Map.lookup k (registers m)
-- --                             m'= m { registers = decOne (registers m) }
-- --                          in ("@" ++ (show (segIndex + n)) ++ "\nD=M\n@" ++ (show sp) ++
-- --                              "\nM=D\n", m')

-- -- writeArithmetic :: Op -> Memory -> (String, Memory)
-- -- writeArithmetic op m = case op of
-- --   Unary u -> writeUnary u m
-- --   Binary (A a) -> writeArith a m
-- --   Binary (B b) -> writeBool b m 

-- -- writeUnary :: UOp -> Memory -> (String, Memory)
-- -- writeUnary u m = let Just sp = Map.lookup "SP" (registers m)
-- --                      m' = m { registers = decOne (registers m) }
-- --                  in ("@" ++ (show sp) ++ "\nM=" ++ (show u) ++ "D\n", m')

-- -- -- i factored this to rely on the fact that the D register is properly set from
-- -- -- the last push operation
-- -- writeArith :: ArithOp -> Memory -> (String, Memory)
-- -- writeArith op m = let Just sp = Map.lookup "SP" (registers m)
-- --                       m' = m { registers = decOne (registers m) }
-- --                   in ("@" ++ (show (sp-2)) ++ "\nM=M" ++ (show op) ++ "D\n", m')


-- -- writeBool :: BoolOp -> Memory -> (String, Memory)
-- -- writeBool b m = let Just sp = Map.lookup "SP" (registers m)
-- --                     i = nextBoolIndex m
-- --                     m' = m { registers = decOne (registers m) , nextBoolIndex = (i + 1)}
-- --                 in ("@" ++ (show (sp-1)) ++ "\n" ++ "D=M\n@" ++ (show (sp-2)) ++
-- --                    "\nD=M-D\n" ++  "@true" ++ (show i) ++ "\nD;" ++ (show b)  ++ "\nM=0\n(true" ++
-- --                    (show i) ++ ")\n@" ++ (show (sp-2)) ++ "\nM=-1\n", m')

-- getRights :: [Either a Command] -> [Command]
-- getRights = rights

-- getCommands :: [String] -> [Command]
-- getCommands xs = getRights (map (regularParse parseCommand) xs)
