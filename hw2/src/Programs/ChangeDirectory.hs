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
import Utils (combineProgram, getDirectory, throwIf, getNewLocalPath)
import Vendor.FilePath (normaliseEx)


data ChangeDirectoryOptions = ChangeDirectoryOptions
    { changeDirectoryTo :: String } deriving (Show)


changeDirectoryParamsParser :: Parser ChangeDirectoryOptions
changeDirectoryParamsParser = ChangeDirectoryOptions
      <$> argument str
          ( metavar "PATH"
         <> help "Change working directory to the PATH" )


changeDirectoryCli :: ParserInfo ChangeDirectoryOptions
changeDirectoryCli = info (changeDirectoryParamsParser <**> helper) ( fullDesc
            <> progDesc "Change working directory to the PATH"
            <> header "cd - a test for optparse-applicative" )


changeDirectoryExecutor :: ChangeDirectoryOptions -> SubprogramEnv (Maybe String)
changeDirectoryExecutor options = do
    appState <- get
    newLocalPath <- getNewLocalPath $ changeDirectoryTo options
    _ <- getDirectory newLocalPath
    lift $ put $ appState {getCurrentStatePath = newLocalPath}
    return Nothing


changeDirectory :: Subprogram
changeDirectory = combineProgram changeDirectoryExecutor changeDirectoryCli
