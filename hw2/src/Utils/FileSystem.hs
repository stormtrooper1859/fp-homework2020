{-# LANGUAGE OverloadedStrings #-}

module Utils.FileSystem (getFileSystem, syncWithFS) where


import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap as HM
import System.Directory
import System.FilePath

import Typings (FileSystem (..))


getFileSystem :: FilePath -> IO FileSystem
getFileSystem path = do
    isDirectory <- doesDirectoryExist path
    isSymlink <- pathIsSymbolicLink path
    if isDirectory then do
        childrens <- listDirectory path
        rez <- mapM (\name -> getFileSystem (path </> name) >>= \t -> return (name, t)) childrens
        let hashMap = HM.fromList rez
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
writeInFS root (Directory _ childrens) = do
    createDirectoryIfMissing False root
    let keysValues = HM.assocs childrens
    _ <- mapM (\(dir, fs) -> writeInFS (root </> dir) fs) keysValues
    return ()
writeInFS root (File _ cnt) = do
    C.writeFile root cnt
writeInFS _ _ = return ()


validateFS :: FilePath -> FileSystem -> IO ()
validateFS _root _snapshotFS = return ()
