{-# LANGUAGE OverloadedStrings #-}

module Main where

import Typings (FileSystem(..), ApplicationContext(..), ApplicationState(..), SubprogramException(..), Subprogram)
import Programs (getProgram)
import Vendor.FilePath (normaliseEx)
import Utils.FileSystem (getFileSystem, syncWithFS)

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

import FileManager (commandLoop)



getRoot :: IO String
getRoot = return "./test_folder"



main :: IO ()
main = do
    args <- getArgs
    -- putStrLn $ concat args
    root <- getRoot
    currentDirectory <- getCurrentDirectory
    let realRoot = normaliseEx $ currentDirectory </> root
    fs <- getFileSystem $ realRoot
    newFileSystem <- runReaderT (commandLoop $ ApplicationState "." fs) (ApplicationContext realRoot fs)
    syncWithFS realRoot fs (getCurrentFileSystem newFileSystem)

