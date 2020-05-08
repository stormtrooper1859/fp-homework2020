{-# LANGUAGE OverloadedStrings #-}

module Utils.FileSystem (getFileSystem, syncWithFS) where

import Typings (ApplicationContext(..), ApplicationState(..), SubprogramException(..), Subprogram, FileSystem(..))
import Programs.ChangeDirectory (changeDirectory)
import Vendor.FilePath (normaliseEx)


import Control.Monad.State
-- import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Maybe
-- import Control.Applicative
import Data.IORef
import System.Environment
import System.Directory
import System.FilePath
import Options.Applicative
import Data.Semigroup ((<>))
import Lib
-- import Control.Monad.Error
import Control.Monad.Except
import Data.List
import System.IO

import Debug.Trace

import Data.HashMap




getFileSystem :: FilePath -> IO FileSystem
getFileSystem path = do
    isDirectory <- doesDirectoryExist path
    isSymlink <- pathIsSymbolicLink path
    if isDirectory then do
        childrens <- listDirectory path
        rez <- mapM (\name -> getFileSystem (path </> name) >>= \t -> return (name, t)) childrens
        let hashMap = fromList rez
        return $ Directory path hashMap
    else if isSymlink then do
        return Stub
    else do
        return $ File $ takeFileName path




syncWithFS :: FilePath -> FileSystem -> FileSystem -> IO ()
syncWithFS root previousFS newFS = do
    validateFS root previousFS
    writeInFS root newFS

writeInFS :: FilePath -> FileSystem -> IO ()
writeInFS root newFS = do
    return ()

validateFS :: FilePath -> FileSystem -> IO ()
validateFS root snapshotFS = return ()





