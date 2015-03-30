module Paths_aencode (
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

bindir     = "/home/nick/.cabal/bin"
libdir     = "/home/nick/.cabal/lib/i386-linux-ghc-7.6.2/aencode-0.1.0.0"
datadir    = "/home/nick/.cabal/share/i386-linux-ghc-7.6.2/aencode-0.1.0.0"
libexecdir = "/home/nick/.cabal/libexec"
sysconfdir = "/home/nick/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aencode_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aencode_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "aencode_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aencode_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aencode_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
