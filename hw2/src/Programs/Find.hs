module Programs.Find
       ( find
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

import Debug.Trace

data ListOptions = ListOptions
  { changeDirectoryTo :: String } deriving (Show)

findParamsParser :: Parser ListOptions
findParamsParser = ListOptions
      <$> argument str
          ( metavar "PATH"
         <> help "Create directory by the PATH" )


findCli :: ParserInfo ListOptions
findCli = info (findParamsParser <**> helper) ( fullDesc
            <> progDesc "Change directory to the TARGET"
            <> header "cd - a test for optparse-applicative" )

findExecutor :: ListOptions -> SubprogramEnv (Maybe String)
findExecutor options = do
    appState <- get
    rootPath <- asks getRootPath
    -- queryPath <- getNewLocalPath $ changeDirectoryTo options
    let queryPath = normaliseEx $ rootPath </> (getCurrentFileSystem appState)
    let queryName = changeDirectoryTo options
    -- let futureFullPath = normaliseEx $ getCurrentStatePath appState </> changeDirectoryTo options
    -- let (diff, nf) = splitFileName $ dropTrailingPathSeparator $ makeRelative rootPath futureFullPath

    -- futureFs <- getDirectory queryPath
    -- let val = lookup queryPath futureFs

    -- let newFs = futureFs{getChildrens = Data.HashMap.insert nf (Directory futureFullPath Data.HashMap.empty) (getChildrens futureFs)}
    -- nfs <- replaceFs diff newFs
    -- lift $ put $ appState {getCurrentFileSystem = nfs}
    return Nothing
    -- return $ Just $ concat $ intersperse "\t" $ sort $ keys $ getChildrens futureFs


find :: Subprogram
find = combineProgram findExecutor findCli




