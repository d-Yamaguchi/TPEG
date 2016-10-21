{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_TPEG (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Yamagutino/Documents/KuramitsuLab/TPEG/.stack-work/install/x86_64-osx/lts-7.4/8.0.1/bin"
libdir     = "/Users/Yamagutino/Documents/KuramitsuLab/TPEG/.stack-work/install/x86_64-osx/lts-7.4/8.0.1/lib/x86_64-osx-ghc-8.0.1/TPEG-0.1.0.0-L62dHqUd5CwGyazUa5HUMt"
datadir    = "/Users/Yamagutino/Documents/KuramitsuLab/TPEG/.stack-work/install/x86_64-osx/lts-7.4/8.0.1/share/x86_64-osx-ghc-8.0.1/TPEG-0.1.0.0"
libexecdir = "/Users/Yamagutino/Documents/KuramitsuLab/TPEG/.stack-work/install/x86_64-osx/lts-7.4/8.0.1/libexec"
sysconfdir = "/Users/Yamagutino/Documents/KuramitsuLab/TPEG/.stack-work/install/x86_64-osx/lts-7.4/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TPEG_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TPEG_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TPEG_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TPEG_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TPEG_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
