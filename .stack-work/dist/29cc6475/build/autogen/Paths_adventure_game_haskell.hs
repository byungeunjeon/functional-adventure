{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_adventure_game_haskell (
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

bindir     = "C:\\Users\\byung\\Desktop\\adventure-game-haskell\\.stack-work\\install\\00831771\\bin"
libdir     = "C:\\Users\\byung\\Desktop\\adventure-game-haskell\\.stack-work\\install\\00831771\\lib\\x86_64-windows-ghc-8.8.3\\adventure-game-haskell-0.1.0.0-IZ3Xpng7jp27n1yJ5KYwUj"
dynlibdir  = "C:\\Users\\byung\\Desktop\\adventure-game-haskell\\.stack-work\\install\\00831771\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\byung\\Desktop\\adventure-game-haskell\\.stack-work\\install\\00831771\\share\\x86_64-windows-ghc-8.8.3\\adventure-game-haskell-0.1.0.0"
libexecdir = "C:\\Users\\byung\\Desktop\\adventure-game-haskell\\.stack-work\\install\\00831771\\libexec\\x86_64-windows-ghc-8.8.3\\adventure-game-haskell-0.1.0.0"
sysconfdir = "C:\\Users\\byung\\Desktop\\adventure-game-haskell\\.stack-work\\install\\00831771\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "adventure_game_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "adventure_game_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "adventure_game_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "adventure_game_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "adventure_game_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "adventure_game_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
