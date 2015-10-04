module Paths_vm (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/martyall/code/nand_to_tetris/projects/07/VM/.stack-work/install/x86_64-osx/lts-2.22/7.8.4/bin"
libdir     = "/Users/martyall/code/nand_to_tetris/projects/07/VM/.stack-work/install/x86_64-osx/lts-2.22/7.8.4/lib/x86_64-osx-ghc-7.8.4/vm-0.1.0.0"
datadir    = "/Users/martyall/code/nand_to_tetris/projects/07/VM/.stack-work/install/x86_64-osx/lts-2.22/7.8.4/share/x86_64-osx-ghc-7.8.4/vm-0.1.0.0"
libexecdir = "/Users/martyall/.cabal/libexec"
sysconfdir = "/Users/martyall/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vm_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "vm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
