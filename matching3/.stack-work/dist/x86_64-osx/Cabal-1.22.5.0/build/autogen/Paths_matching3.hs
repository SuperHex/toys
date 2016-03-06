module Paths_matching3 (
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

bindir     = "/Users/clinix/Documents/Codes/toys/matching3/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/bin"
libdir     = "/Users/clinix/Documents/Codes/toys/matching3/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/lib/x86_64-osx-ghc-7.10.3/matching3-0.1.0.0-3D4AWVX6UIJBlmnZcHE0R6"
datadir    = "/Users/clinix/Documents/Codes/toys/matching3/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/share/x86_64-osx-ghc-7.10.3/matching3-0.1.0.0"
libexecdir = "/Users/clinix/Documents/Codes/toys/matching3/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/libexec"
sysconfdir = "/Users/clinix/Documents/Codes/toys/matching3/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "matching3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "matching3_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "matching3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "matching3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "matching3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
