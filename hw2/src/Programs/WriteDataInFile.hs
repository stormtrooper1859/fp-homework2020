module Programs.WriteDataInFile
       ( writeDataInFile
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
  { textData :: String, changeDirectoryTo :: String } deriving (Show)

writeDataInFileParamsParser :: Parser ListOptions
writeDataInFileParamsParser = ListOptions
    <$> argument str
        ( metavar "Text"
        <> help "Create directory by the PATH" )
    <*> strOption
        ( long "to"
        <> metavar "TARGET"
        <> help "Target for the greeting" )


writeDataInFileCli :: ParserInfo ListOptions
writeDataInFileCli = info (writeDataInFileParamsParser <**> helper) ( fullDesc
            <> progDesc "Change directory to the TARGET"
            <> header "cd - a test for optparse-applicative" )


writeDataInFileExecutor :: ListOptions -> SubprogramEnv (Maybe String)
writeDataInFileExecutor options = do
    appState <- get
    rootPath <- asks getRootPath
    let (diff1, nf) = splitFileName $ dropTrailingPathSeparator $ normalise $ changeDirectoryTo options
    diff <- getNewLocalPath diff1

    -- let localPath = getCurrentStatePath appState
    -- let futureFullPath = normaliseEx $ rootPath </> localPath </> changeDirectoryTo options
    -- let (diff, nf) = splitFileName $ dropTrailingPathSeparator $ makeRelative rootPath futureFullPath
    futureFs <- getDirectory diff
    let newFs = futureFs{getChildrens = Data.HashMap.insert nf (File (normalise $ rootPath </> diff) (C.pack $ textData options)) (getChildrens futureFs)}
    nfs <- replaceFs diff newFs
    lift $ put $ appState {getCurrentFileSystem = nfs}
    return Nothing


writeDataInFile :: Subprogram
writeDataInFile = combineProgram writeDataInFileExecutor writeDataInFileCli
