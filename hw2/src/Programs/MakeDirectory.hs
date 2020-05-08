module Programs.MakeDirectory
       ( makeDirectory
       ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Reader
import qualified Data.HashMap as HM
import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath

import Typings (ApplicationContext (..), ApplicationState (..), FileSystem (..), Subprogram,
                SubprogramEnv, SubprogramException (..))
import Utils (combineProgram, getDirectory, throwIf, getNewLocalPath, replaceFs)
import Vendor.FilePath (normaliseEx)


data ListOptions = ListOptions { changeDirectoryTo :: String } deriving (Show)


makeDirectoryParamsParser :: Parser ListOptions
makeDirectoryParamsParser = ListOptions
    <$> argument str
        ( metavar "PATH"
        <> help "Create directory by the PATH" )


makeDirectoryCli :: ParserInfo ListOptions
makeDirectoryCli = info (makeDirectoryParamsParser <**> helper) ( fullDesc
    <> progDesc "Change directory to the TARGET"
    <> header "cd - a test for optparse-applicative" )


makeDirectoryExecutor :: ListOptions -> SubprogramEnv (Maybe String)
makeDirectoryExecutor options = do
    appState <- get
    rootPath <- asks getRootPath
    let (pathToFolderQ, folderName) = splitFileName $ dropTrailingPathSeparator $ normalise $ changeDirectoryTo options
    pathToFolder <- getNewLocalPath pathToFolderQ
    folder <- getDirectory pathToFolder
    throwIf (HM.member folderName $ getChildrens folder) $ SubprogramRuntimeException "Объект уже существует"
    let directoryPath = normaliseEx $ rootPath </> pathToFolderQ </> folderName
    let folderWithNew = folder{getChildrens = HM.insert folderName (Directory directoryPath HM.empty) (getChildrens folder)}
    newFileSystem <- replaceFs pathToFolderQ folderWithNew
    lift $ put $ appState {getCurrentFileSystem = newFileSystem}
    return Nothing


makeDirectory :: Subprogram
makeDirectory = combineProgram makeDirectoryExecutor makeDirectoryCli
