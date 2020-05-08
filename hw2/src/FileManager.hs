{-# LANGUAGE OverloadedStrings #-}

module FileManager
       ( commandLoop
       ) where


import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import System.FilePath
import System.IO

import Programs (getProgram)
import Typings (ApplicationContext (..), ApplicationState (..), SubprogramEnv,
                SubprogramException (..))
import Vendor.FilePath (normaliseEx)


resultHandler :: (Either SubprogramException (Maybe String), ApplicationState) -> ApplicationState -> IO ApplicationState
resultHandler ((Left (SubprogramArgumentsException text)), _) dflt = do
    putStrLn $ text
    return dflt
resultHandler ((Left (SubprogramRuntimeException text)), _) dflt = do
    putStrLn $ "Ошибочка вышла: " ++ text
    return dflt
resultHandler ((Right Nothing), st) _ = do
    return st
resultHandler ((Right (Just a)), st) _ = do
    putStrLn a
    return st


executeProgram :: ApplicationState -> ([String] -> SubprogramEnv (Maybe String)) -> [String] -> ReaderT ApplicationContext IO ApplicationState
executeProgram appState program args = do
    context <- ask
    let returnedValue = runState (runExceptT $ runReaderT (program args) context) appState
    newState <- lift $ resultHandler returnedValue appState
    return newState


commandLoop :: ApplicationState -> ReaderT ApplicationContext IO ApplicationState
commandLoop appState = do
    context <- ask
    let root = getRootPath context
    let relPath = getCurrentStatePath appState
    lift $ putStr $ (normaliseEx (root </> relPath)) ++ " > "
    lift $ hFlush stdout
    line <- lift getLine
    if line == "" then
        commandLoop appState
    else do
        let (progName : args) = words line
        case progName of
            "q" -> do
                return $ appState
            _ -> do
                let prog = getProgram progName
                newState <- executeProgram appState (prog progName) args
                commandLoop newState
