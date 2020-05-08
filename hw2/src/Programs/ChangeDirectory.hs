module Programs.ChangeDirectory
       ( changeDirectory
       ) where

import Control.Monad.State
import Data.Semigroup ((<>))
import Options.Applicative

import Typings (ApplicationState (..), Subprogram, SubprogramEnv)
import Utils (combineProgram, getDirectory, getNewLocalPath)


data ChangeDirectoryOptions = ChangeDirectoryOptions { queryPath :: String } deriving (Show)


changeDirectoryParamsParser :: Parser ChangeDirectoryOptions
changeDirectoryParamsParser = ChangeDirectoryOptions
    <$> argument str
        ( metavar "PATH"
        <> help "Change working directory to the PATH" )


changeDirectoryCli :: ParserInfo ChangeDirectoryOptions
changeDirectoryCli = info (changeDirectoryParamsParser <**> helper) ( fullDesc
    <> progDesc "Change working directory to the PATH"
    <> header "cd - utility for changing current working directory" )


changeDirectoryExecutor :: ChangeDirectoryOptions -> SubprogramEnv (Maybe String)
changeDirectoryExecutor options = do
    appState <- get
    newLocalPath <- getNewLocalPath $ queryPath options
    _ <- getDirectory newLocalPath
    lift $ put $ appState {getCurrentStatePath = newLocalPath}
    return Nothing


changeDirectory :: Subprogram
changeDirectory = combineProgram changeDirectoryExecutor changeDirectoryCli
