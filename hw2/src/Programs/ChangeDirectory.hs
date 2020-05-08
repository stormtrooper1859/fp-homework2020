module Programs.ChangeDirectory
       ( changeDirectory
       ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Reader
import Data.HashMap
import Data.List
import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath

import Typings (ApplicationContext (..), ApplicationState (..), FileSystem (..), Subprogram,
                SubprogramEnv, SubprogramException (..))
import Utils (combineProgram)
import Vendor.FilePath (normaliseEx)


data ChangeDirectoryOptions = ChangeDirectoryOptions
  { changeDirectoryTo :: String } deriving (Show)

changeDirectoryParamsParser :: Parser ChangeDirectoryOptions
changeDirectoryParamsParser = ChangeDirectoryOptions
      <$> argument str
          ( metavar "DIRECTORY"
         <> help "Change directory to the DIRECTORY" )


changeDirectoryCli :: ParserInfo ChangeDirectoryOptions
changeDirectoryCli = info (changeDirectoryParamsParser <**> helper) ( fullDesc
            <> progDesc "Change directory to the TARGET"
            <> header "cd - a test for optparse-applicative" )


-- проверяем что остались в рабочем каталоге
isPathCorrect :: FilePath -> Bool
isPathCorrect diff = not (isRelative diff) || (isPrefixOf (".." ++ [pathSeparator]) diff) || (".." == diff)

-- проверяем что данный путь существует в нашем рабочем каталоге
isPathExist :: FileSystem -> FilePath -> Bool
isPathExist fs filePath = isPathExistInternal fs $ splitDirectories filePath
    where
        isPathExistInternal dir@(Directory _ _) ("." : xs) = isPathExistInternal dir xs
        isPathExistInternal (Directory _ children) (x : xs) = isPathExistInternal (findWithDefault Stub x children) xs
        isPathExistInternal (Directory _ _) [] = True
        isPathExistInternal _ _ = False


changeDirectoryExecutor :: ChangeDirectoryOptions -> SubprogramEnv (Maybe String)
changeDirectoryExecutor options = do
    appState <- get
    rootPath <- asks getRootPath
    let futureFullPath = normaliseEx $ getCurrentStatePath appState </> changeDirectoryTo options
    let diff = makeRelative rootPath futureFullPath
    if isPathCorrect diff then
        throwError $ SubprogramRuntimeException "Нельзя покинуть рабочий каталог"
    else do
        if isPathExist (getCurrentFileSystem appState) diff then do
            lift $ put $ appState {getCurrentStatePath = normaliseEx $ getCurrentStatePath appState </> changeDirectoryTo options}
            return Nothing
        else
            throwError $ SubprogramRuntimeException "Заданный путь не существует"


changeDirectory :: Subprogram
changeDirectory = combineProgram changeDirectoryExecutor changeDirectoryCli
