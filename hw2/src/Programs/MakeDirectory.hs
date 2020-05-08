module Programs.MakeDirectory
       ( makeDirectory
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
import Utils (combineProgram, getDirectory, throwIf, getNewLocalPath, replaceFs)
import Vendor.FilePath (normaliseEx)

import Debug.Trace

data ListOptions = ListOptions
  { changeDirectoryTo :: String } deriving (Show)

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
    let (diff1, nf) = splitFileName $ dropTrailingPathSeparator $ normalise $ changeDirectoryTo options
    diff <- getNewLocalPath diff1

    -- let localPath = getCurrentStatePath appState
    -- let futureFullPath = normaliseEx $ rootPath </> localPath </> changeDirectoryTo options
    -- let (diff, nf) = splitFileName $ dropTrailingPathSeparator $ makeRelative rootPath futureFullPath
    futureFs <- getDirectory diff
    let newFs = futureFs{getChildrens = Data.HashMap.insert nf (Directory (normalise $ rootPath </> diff) Data.HashMap.empty) (getChildrens futureFs)}
    nfs <- replaceFs diff newFs
    lift $ put $ appState {getCurrentFileSystem = nfs}
    return Nothing


makeDirectory :: Subprogram
makeDirectory = combineProgram makeDirectoryExecutor makeDirectoryCli
