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
import Lib
-- import Control.Monad.Error
import Control.Monad.Except
import Data.List
import System.IO

import Debug.Trace



executeProgram :: ApplicationState -> Subprogram -> [String] -> ReaderT ApplicationContext IO ApplicationState
executeProgram state program args = do
    context <- ask
    let newState1 = runState (runExceptT $ runReaderT (program args) context) state
    -- let newState = execState (runExceptT $ cdCommandWrapper2 params) state
    newState <- lift $ smth newState1 state
    -- lift $ putStrLn $ getCurrentStatePath newState
    return newState


commandLoop :: ApplicationState -> ReaderT ApplicationContext IO ApplicationState
commandLoop state = do
    context <- ask
    let root = getRootPath context
    -- relPath <- lift $ readIORef $ currentPath ApplicationContext
    let relPath = getCurrentStatePath state
    lift $ putStr $ (normaliseEx (root </> relPath)) ++ " > "
    lift $ hFlush stdout
    line <- lift getLine
    if
        line == ""
    then 
        commandLoop state
    else do
        let (x : xs) = words line
        case x of
            "q" -> do
                t2 <- ask
                return $ state
            _ -> do
                let prog = getProgram x
                newState <- executeProgram state prog xs
                commandLoop newState


smth :: (Either SubprogramException (Maybe String), ApplicationState) -> ApplicationState -> IO ApplicationState
smth ((Left (SubprogramArgumentsException text)), state) dflt = do
    putStrLn $ "ошибочка в аргументах: " ++ text
    return dflt
smth ((Left (SubprogramRuntimeException text)), state) dflt = do
    putStrLn $ "ошибочка вышла: " ++ text
    return dflt
smth ((Right Nothing), state) _ = do
    return state
smth ((Right (Just a)), state) _ = do
    putStrLn a
    return state


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

