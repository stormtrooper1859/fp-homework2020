module Utils
       ( parserPrefs
       , combineProgram
       , getDirectory
       , replaceFs
       , throwIf
       , isPathOutside
       , getNewLocalPath
       , getDefault
       , throwMaybe
       , isFile
       ) where

import Options.Applicative

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Reader
import Data.HashMap
import Data.List
import System.FilePath

import Typings (ApplicationContext (..), ApplicationState (..), FileSystem (..), Subprogram,
                SubprogramEnv, SubprogramException (..))
import Vendor.FilePath (normaliseEx)


parserPrefs :: ParserPrefs
parserPrefs = ParserPrefs "" False False False False 1


-- принимает аргументы и запускает на них парсер и передает в параметры программы
combineProgram :: (Show a) => (a -> SubprogramEnv (Maybe String)) -> ParserInfo a -> Subprogram
combineProgram program parser progName = \args -> do
    let params = execParserPure parserPrefs parser args
    resultWrapper program params progName


-- обрабатываем результат парсера
resultWrapper :: (Show a) => (a -> SubprogramEnv (Maybe String)) -> ParserResult a -> String -> SubprogramEnv (Maybe String)
resultWrapper program (Success a) _ = program a
resultWrapper _ (Failure msg) progName = do
    let (helpMessage, _, _) = execFailure msg progName
    throwError $ SubprogramArgumentsException $ show helpMessage
resultWrapper _ f _ = throwError $ SubprogramArgumentsException $ show f


getDirectory :: FilePath -> SubprogramEnv FileSystem
getDirectory filePath = do
    fs <- gets getCurrentFileSystem
    isPathExistInternal fs $ splitDirectories filePath
    where
        isPathExistInternal :: FileSystem -> [String] -> SubprogramEnv FileSystem
        isPathExistInternal dir@(Directory _ _) ("." : xs) = isPathExistInternal dir xs
        isPathExistInternal (Directory _ children) (x : xs) = isPathExistInternal (findWithDefault Symlink x children) xs
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
            let k = findWithDefault Symlink currentKey children
            t <- replaceFsInternal k restPath
            return dir{getChildrens = Data.HashMap.insert currentKey t children}
        replaceFsInternal _ [] = return newFolder
        replaceFsInternal _ _ = throwError $ SubprogramRuntimeException "Заданный путь не существует"


throwIf :: Bool -> SubprogramException -> SubprogramEnv ()
throwIf condition err = if condition then throwError err else return ()


throwMaybe :: Maybe a -> SubprogramException -> SubprogramEnv a
throwMaybe (Just a) _  = return a
throwMaybe Nothing err = throwError err


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
    throwIf (not (isValid newLocalPath)) $ SubprogramRuntimeException "Запрещенное название пути"
    throwIf (isPathOutside newLocalPath) $ SubprogramRuntimeException "Нельзя покинуть рабочий каталог"
    return newLocalPath


getDefault :: Maybe a -> a -> a
getDefault Nothing a  = a
getDefault (Just a) _ = a


isFile :: FileSystem -> Bool
isFile (File _ _) = True
isFile _          = False
