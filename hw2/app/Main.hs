{-# LANGUAGE OverloadedStrings #-}

module Main where
-- import Control.Monad
-- import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.Maybe
-- import Control.Applicative
import Data.IORef
import System.Environment
import System.Directory
import System.FilePath

-- import Lib

-- программа пишет текущий каталог, изменяет размер переменной, пишет его занчение

data Env = Env { rootPath :: FilePath
                , currentPath :: IORef String
                , storedValue :: IORef Int }

commandLoop :: ReaderT Env IO Env
commandLoop = do
    env <- ask
    let root = rootPath env
    relPath <- lift $ readIORef $ currentPath env
    lift $ putStr $ (normalise (root </> relPath)) ++ " > "
    t <- lift getLine
    if
        t == "q"
    then do
        t2 <- ask
        return t2
    else
        commandLoop



getRoot :: IO String
getRoot = return "./hw1/test"


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
    t <- runReaderT (commandLoop) (Env realRoot refRoot zero)
    rez <- readIORef $ currentPath t
    return ()
    -- putStrLn $ rez
