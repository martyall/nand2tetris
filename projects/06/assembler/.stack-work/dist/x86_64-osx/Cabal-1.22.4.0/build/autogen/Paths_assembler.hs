module Paths_assembler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/martyall/code/nand_to_tetris/projects/06/assembler/.stack-work/install/x86_64-osx/nightly-2015-08-11/7.10.2/bin"
libdir     = "/Users/martyall/code/nand_to_tetris/projects/06/assembler/.stack-work/install/x86_64-osx/nightly-2015-08-11/7.10.2/lib/x86_64-osx-ghc-7.10.2/assembler-0.1.0.0-1i4FetmHLei1pUKr3GXLtZ"
datadir    = "/Users/martyall/code/nand_to_tetris/projects/06/assembler/.stack-work/install/x86_64-osx/nightly-2015-08-11/7.10.2/share/x86_64-osx-ghc-7.10.2/assembler-0.1.0.0"
libexecdir = "/Users/martyall/.cabal/libexec"
sysconfdir = "/Users/martyall/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "assembler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "assembler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "assembler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "assembler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "assembler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
