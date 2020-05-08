module Programs.Cat
       ( cat
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

catParamsParser :: Parser ListOptions
catParamsParser = ListOptions
      <$> argument str
          ( metavar "PATH"
         <> help "Create directory by the PATH" )


catCli :: ParserInfo ListOptions
catCli = info (catParamsParser <**> helper) ( fullDesc
            <> progDesc "Change directory to the TARGET"
            <> header "cd - a test for optparse-applicative" )


catExecutor :: ListOptions -> SubprogramEnv (Maybe String)
catExecutor options = do
    appState <- get
    rootPath <- asks getRootPath
    let (diff1, nf) = splitFileName $ normalise $ changeDirectoryTo options
    diff <- getNewLocalPath diff1

    futureFs <- getDirectory diff
    let ht = getChildrens futureFs
    let file = Data.HashMap.lookup nf ht
    -- res <- show $ fileContent file
    return (file >>= \f -> return $ show $ fileContent f)


cat :: Subprogram
cat = combineProgram catExecutor catCli
