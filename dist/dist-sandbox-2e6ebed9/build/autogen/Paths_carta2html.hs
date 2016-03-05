module Paths_carta2html (
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

bindir     = "/Users/egiraldo23/Documents/GitHub/carta2html/.cabal-sandbox/bin"
libdir     = "/Users/egiraldo23/Documents/GitHub/carta2html/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/carta2html-0.1.0.0-6iN9qxEeq3mIy5Ozaj0KKD"
datadir    = "/Users/egiraldo23/Documents/GitHub/carta2html/.cabal-sandbox/share/x86_64-osx-ghc-7.10.2/carta2html-0.1.0.0"
libexecdir = "/Users/egiraldo23/Documents/GitHub/carta2html/.cabal-sandbox/libexec"
sysconfdir = "/Users/egiraldo23/Documents/GitHub/carta2html/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "carta2html_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "carta2html_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "carta2html_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "carta2html_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "carta2html_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
