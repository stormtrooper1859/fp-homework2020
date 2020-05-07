{-# LANGUAGE OverloadedStrings #-}

module Main where

import Typings (ApplicationContext(..), ApplicationState(..), CDException(..), Subprogram)
import Programs.ChangeDirectory (cdOpts, cdCommandWrapper2, changeDirectory)
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
import Control.Monad.Error
import Data.List
import System.IO

import Debug.Trace


defaultProgram :: Subprogram
defaultProgram _ = return ""

-- getProgram :: String -> ReaderT ApplicationContext ExceptT (State ApplicationState ()) 
getProgram :: String -> Subprogram
getProgram x = case x of
            "cd" -> changeDirectory
            _ -> defaultProgram



executeProgram :: ApplicationState -> Subprogram -> [String] -> ReaderT ApplicationContext IO ApplicationState
executeProgram state program args = do
    context <- ask
    let newState1 = runState (runExceptT $ runReaderT (program args) context) state
    -- let newState = execState (runExceptT $ cdCommandWrapper2 params) state
    newState <- lift $ smth newState1 state
    lift $ putStrLn $ currentStatePath newState
    return newState


commandLoop :: ApplicationState -> ReaderT ApplicationContext IO ()
commandLoop state = do
    context <- ask
    let root = getRootPath context
    -- relPath <- lift $ readIORef $ currentPath ApplicationContext
    let relPath = currentStatePath state
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
                return ()
            _ -> do
                lift $ putStrLn $ "im here " ++ x
                let prog = getProgram x
                newState <- executeProgram state prog xs
                commandLoop newState


smth :: (Either CDException String, ApplicationState) -> ApplicationState -> IO ApplicationState
smth ((Left (SubprogramException text)), state) dflt = do
    putStrLn $ "ошибочка вышла: " ++ text
    return dflt
smth ((Right a), state) _ = do
    putStrLn a
    return state

getRoot :: IO String
getRoot = return "./hw1/test"

defaultCalcState :: ApplicationState
defaultCalcState = ApplicationState "."

-- smt :: String -> Int
-- smt line = do
--     let (x : xs) = words line
--     case x of
--         "quit" -> 1
--         "cd" -> 2



-- data Metainfo = Metainfo { editDate :: String }

data FileSystem = Directory { directoryName :: String, getChildrens :: [FileSystem] } | File { fileName :: String } deriving (Show)

getFileSystem :: FilePath -> IO FileSystem
getFileSystem path = do
    isDirectory <- doesDirectoryExist path
    if not isDirectory then do
        return $ File $ takeFileName path
    else do
        childrens <- listDirectory path
        rez <- mapM (getFileSystem . (path </>)) childrens
        return $ Directory path rez

main :: IO ()
main = do
    args <- getArgs
    -- putStrLn $ concat args
    root <- getRoot
    currentDirectory <- getCurrentDirectory
    let realRoot = currentDirectory </> root
    fs <- getFileSystem $ realRoot
    putStrLn $ show fs
    refRoot <- newIORef "."
    zero <- newIORef 0
    runReaderT (commandLoop defaultCalcState) (ApplicationContext realRoot refRoot zero)
    -- rez <- readIORef $ currentPath t
    -- return ()
    -- putStrLn $ rez








-- parserPrefs :: ParserPrefs
-- parserPrefs = ParserPrefs "" False False False False 1


