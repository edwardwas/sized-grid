{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_sized_grid (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/edward/git_repos/haskell/sized-grid/.stack-work/install/x86_64-linux/lts-11.2/8.2.2/bin"
libdir     = "/home/edward/git_repos/haskell/sized-grid/.stack-work/install/x86_64-linux/lts-11.2/8.2.2/lib/x86_64-linux-ghc-8.2.2/sized-grid-0.1.0.0-E6vI8hNhPq9C6SVt6usrNZ"
dynlibdir  = "/home/edward/git_repos/haskell/sized-grid/.stack-work/install/x86_64-linux/lts-11.2/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/edward/git_repos/haskell/sized-grid/.stack-work/install/x86_64-linux/lts-11.2/8.2.2/share/x86_64-linux-ghc-8.2.2/sized-grid-0.1.0.0"
libexecdir = "/home/edward/git_repos/haskell/sized-grid/.stack-work/install/x86_64-linux/lts-11.2/8.2.2/libexec/x86_64-linux-ghc-8.2.2/sized-grid-0.1.0.0"
sysconfdir = "/home/edward/git_repos/haskell/sized-grid/.stack-work/install/x86_64-linux/lts-11.2/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sized_grid_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sized_grid_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sized_grid_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sized_grid_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sized_grid_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sized_grid_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
