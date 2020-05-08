module Utils
       ( parserPrefs
       , combineProgram
       , getDirectory
       , replaceFs
       , throwIf
       , isPathOutside
       , getNewLocalPath
       ) where

import Options.Applicative

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.IORef
import Data.List
import Data.Semigroup ((<>))
import Lib
import System.Directory
import System.Environment
import System.FilePath
import Data.HashMap

import Debug.Trace

import Typings (ApplicationContext (..), ApplicationState (..), SubprogramException (..), SubprogramEnv, Subprogram, FileSystem(..) )
import Vendor.FilePath (normaliseEx)

parserPrefs :: ParserPrefs
parserPrefs = ParserPrefs "" False False False False 1



combineProgram :: (Show a) => (a -> SubprogramEnv (Maybe String)) -> ParserInfo a -> Subprogram
combineProgram program parser = \args -> do
    let params = execParserPure parserPrefs parser args
    cdCommandWrapper2 program params


cdCommandWrapper2 :: (Show a) => (a -> SubprogramEnv (Maybe String)) -> ParserResult a -> SubprogramEnv (Maybe String)
cdCommandWrapper2 program (Success a) = program a
    -- return $ Nothing
cdCommandWrapper2 program f = throwError $ SubprogramArgumentsException $ show f


getDirectory :: FilePath -> SubprogramEnv FileSystem
getDirectory filePath = do
    fs <- gets getCurrentFileSystem
    isPathExistInternal fs $ splitDirectories filePath
    where
        isPathExistInternal :: FileSystem -> [String] -> SubprogramEnv FileSystem
        isPathExistInternal dir@(Directory _ _) ("." : xs) = isPathExistInternal dir xs
        isPathExistInternal (Directory _ children) (x : xs) = isPathExistInternal (findWithDefault Stub x children) xs
        isPathExistInternal dir@(Directory _ _) [] = return dir
        isPathExistInternal _ _ = throwError $ SubprogramRuntimeException "Заданный путь не существует"


replaceFs :: FilePath -> FileSystem -> SubprogramEnv FileSystem
replaceFs filePath newFolder = do
    fs <- gets getCurrentFileSystem
    replaceFsInternal fs $ splitDirectories filePath
    where
        replaceFsInternal :: FileSystem -> [String] -> SubprogramEnv FileSystem
        replaceFsInternal dir@(Directory _ _) ("." : restPath) = replaceFsInternal dir restPath
        replaceFsInternal dir@(Directory _ children) (currentKey : restPath) = do
            let k = findWithDefault Stub currentKey children
            t <- replaceFsInternal k restPath
            return dir{getChildrens = Data.HashMap.insert currentKey t children}
        replaceFsInternal dir [] = return newFolder
        replaceFsInternal _ _ = throwError $ SubprogramRuntimeException "Заданный путь не существует"


throwIf :: Bool -> SubprogramException -> SubprogramEnv ()
throwIf condition error = if condition then throwError error else return ()


-- проверяем что остались в рабочем каталоге
isPathOutside :: FilePath -> Bool
isPathOutside diff = not (isRelative diff) || (isPrefixOf (".." ++ [pathSeparator]) diff) || (".." == diff)


getNewLocalPath :: FilePath -> SubprogramEnv FilePath
getNewLocalPath path = do
    appState <- get
    rootPath <- asks getRootPath
    let localPath = getCurrentStatePath appState
    let currentFullPath = normaliseEx $ rootPath </> localPath
    let futureFullPath = normaliseEx $ currentFullPath </> path
    let newLocalPath = makeRelative rootPath futureFullPath 
    throwIf (isPathOutside newLocalPath) $ SubprogramRuntimeException "Нельзя покинуть рабочий каталог"
    return newLocalPath
