module Main where

import Scratch2
import Lib
import Parser
--import CodeWriter
import System.IO
import System.Environment
import Text.Parsec (ParseError)
import Data.Either
import Text.Parsec (runParser)
import System.FilePath
import System.Directory
import Control.Monad
import Data.Functor
import Control.Monad.Trans.State.Lazy
import Data.Traversable hiding (sequence)

-- writeCommands :: Memory -> Commands -> ([String], Memory)
-- writeCommands m cs  = case (runParser scriptParser m "" cs) of
--   Right y -> y
--   Left _ -> ([],m)


writeCommands :: Commands -> State Memory [String]
writeCommands cs = do
  mem <- get
  case runParser scriptParser mem "" cs of
    Right (output, mem') -> do
      put mem'
      return output
    Left _ -> return []

getCommands :: [String] -> Commands
getCommands = rights . map (regularParse parseCommand)


-- this assumes the input is a singled file (in line form)
translate :: [String] -> State Memory [String]
translate = writeCommands . getCommands

bootStrap = "@256\nD=A\n@SP\nM=D\n@Sys.init\n0;JMP\n\n"


getVmFiles :: IO [FilePath]
getVmFiles = do
  dir <- getCurrentDirectory
  files <- getDirectoryContents dir
  let getFilesBool = (\x -> ((takeExtension x) == ".vm" && x /= "Sys.vm" ))
  return $ filter getFilesBool files




--this assumes the input is an unlined single program
translateVMFile :: FilePath -> String -> State Memory String
translateVMFile path contents = do
  modify (\m -> m { fileName = path })
  result <- unlines <$> translate (lines contents)
  modify (\m -> m {staticBase = (staticBase m) + (staticOffset m)})
  modify (\m -> m { staticOffset = 0 })
  return result


main :: IO ()
main = do
  vmFiles <- fmap (\paths -> "Sys.vm" : paths) getVmFiles
  stateActions <- traverse translatePath vmFiles
  let programsState = sequence stateActions :: State Memory [String]
      outputs = evalState programsState ram :: [String]
  writeFile "program.asm" $ bootStrap ++ (unlines outputs)
  where
    translatePath :: String -> IO (State Memory String)
    translatePath path = translateVMFile path <$> readFile path


  -- translatedVmFiles <- mapM (translateVMFile) vmFiles
  -- writeFile "program.asm" $ concat ((bootStrap ++ sys):translatedVmFiles)

-- translateVMFile :: FilePath -> IO String
-- translateVMFile file = do
--   content <- readFile file
--   let m = ram { fileName = dropExtension file }
--   return (unlines $ translate m $ lines content)
