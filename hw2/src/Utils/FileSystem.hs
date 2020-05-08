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
-- import Control.Monad.Error
import Control.Monad.Except
import Data.List
import System.IO

import Debug.Trace

import Data.HashMap
import qualified Data.ByteString.Char8 as C



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
        return Symlink
    else do
        fileData <- C.readFile path
        return $ File (takeFileName path) fileData


syncWithFS :: FilePath -> FileSystem -> FileSystem -> IO ()
syncWithFS root previousFS newFS = do
    validateFS root previousFS
    writeInFS root newFS

writeInFS :: FilePath -> FileSystem -> IO ()
writeInFS root (Directory nm ch) = do
    createDirectoryIfMissing False root
    let ks = Data.HashMap.assocs ch
    mapM (\(dir, fs) -> writeInFS (root </> dir) fs) ks
    return ()
writeInFS root (File nm cnt) = do
    C.writeFile root cnt
    -- createDirectoryIfMissing False root
    -- return ()
writeInFS _ _ = return ()


validateFS :: FilePath -> FileSystem -> IO ()
validateFS root snapshotFS = return ()





