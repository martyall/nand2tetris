module Paths_compiler (
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

bindir     = "/Users/martyall/code/nand2tetris/projects/10/compiler/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/bin"
libdir     = "/Users/martyall/code/nand2tetris/projects/10/compiler/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/lib/x86_64-osx-ghc-7.10.2/compiler-0.1.0.0-67JlvSkZUoW2a5wLn384Tq"
datadir    = "/Users/martyall/code/nand2tetris/projects/10/compiler/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/share/x86_64-osx-ghc-7.10.2/compiler-0.1.0.0"
libexecdir = "/Users/martyall/code/nand2tetris/projects/10/compiler/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/libexec"
sysconfdir = "/Users/martyall/code/nand2tetris/projects/10/compiler/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "compiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "compiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "compiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "compiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "compiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
