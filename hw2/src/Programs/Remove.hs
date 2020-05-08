module Programs.Remove
       ( remove
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

import qualified Data.ByteString.Char8 as C

data ListOptions = ListOptions
  { changeDirectoryTo :: String } deriving (Show)

removeParamsParser :: Parser ListOptions
removeParamsParser = ListOptions
    <$> argument str
        ( metavar "Text"
        <> help "Create directory by the PATH" )


removeCli :: ParserInfo ListOptions
removeCli = info (removeParamsParser <**> helper) ( fullDesc
            <> progDesc "Change directory to the TARGET"
            <> header "cd - a test for optparse-applicative" )


removeExecutor :: ListOptions -> SubprogramEnv (Maybe String)
removeExecutor options = do
    appState <- get
    rootPath <- asks getRootPath
    let (diff1, nf) = splitFileName $ dropTrailingPathSeparator $ normalise $ changeDirectoryTo options
    diff <- getNewLocalPath diff1

    -- let localPath = getCurrentStatePath appState
    -- let futureFullPath = normaliseEx $ rootPath </> localPath </> changeDirectoryTo options
    -- let (diff, nf) = splitFileName $ dropTrailingPathSeparator $ makeRelative rootPath futureFullPath
    futureFs <- getDirectory diff
    throwIf (Data.HashMap.notMember nf (getChildrens futureFs)) $ SubprogramRuntimeException "Удаляемый файл отсутствует"
    let newFs = futureFs{getChildrens = Data.HashMap.delete nf (getChildrens futureFs)}
    nfs <- replaceFs diff newFs
    lift $ put $ appState {getCurrentFileSystem = nfs}
    return Nothing


remove :: Subprogram
remove = combineProgram removeExecutor removeCli
