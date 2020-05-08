module Programs.Remove
       ( remove
       ) where


import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap as HM
import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath

import Typings (ApplicationState (..), FileSystem (..), Subprogram, SubprogramEnv,
                SubprogramException (..))
import Utils (combineProgram, getDirectory, getNewLocalPath, replaceFs, throwIf)


data RemoveOptions = RemoveOptions { queryPath :: String } deriving (Show)


removeParamsParser :: Parser RemoveOptions
removeParamsParser = RemoveOptions
    <$> argument str
        ( metavar "PATH"
        <> help "Forced remove object by PATH" )


removeCli :: ParserInfo RemoveOptions
removeCli = info (removeParamsParser <**> helper) ( fullDesc
    <> progDesc "Remove file/directory"
    <> header "rm - utility for removing directory and files" )


removeExecutor :: RemoveOptions -> SubprogramEnv (Maybe String)
removeExecutor options = do
    appState <- get
    let (folderQuery, name) = splitFileName $ dropTrailingPathSeparator $ normalise $ queryPath options
    folderPath <- getNewLocalPath folderQuery
    folder <- getDirectory folderPath
    throwIf (HM.notMember name (getChildrens folder)) $ SubprogramRuntimeException "Удаляемый файл отсутствует"
    let newFolder = folder{getChildrens = HM.delete name (getChildrens folder)}
    newFS <- replaceFs folderPath newFolder
    lift $ put $ appState {getCurrentFileSystem = newFS}
    return Nothing


remove :: Subprogram
remove = combineProgram removeExecutor removeCli
