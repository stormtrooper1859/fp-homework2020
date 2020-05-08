module Programs.List
       ( list
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
  { queryOptionsPath :: String } deriving (Show)

listParamsParser :: Parser ListOptions
listParamsParser = ListOptions
      <$> argument str
          ( metavar "DIRECTORY"
         <> help "Change directory to the DIRECTORY" )


listCli :: ParserInfo ListOptions
listCli = info (listParamsParser <**> helper) ( fullDesc
            <> progDesc "Change directory to the TARGET"
            <> header "cd - a test for optparse-applicative" )


listExecutor :: ListOptions -> SubprogramEnv (Maybe String)
listExecutor options = do
    appState <- get
    queryPath <- getNewLocalPath $ queryOptionsPath options
    queryFS <- getDirectory queryPath
    return $ Just $ concat $ intersperse "\t" $ sort $ keys $ getChildrens queryFS


list :: Subprogram
list = combineProgram listExecutor listCli
