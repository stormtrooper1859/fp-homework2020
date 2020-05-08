module Programs.Cat
       ( cat
       ) where

import Control.Monad.State
import Control.Monad.Trans.Reader
import Data.HashMap
import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath

import Typings (ApplicationContext (..), ApplicationState (..), FileSystem (..), Subprogram,
                SubprogramEnv, SubprogramException (..))
import Utils (combineProgram, getDirectory, throwIf, getNewLocalPath, replaceFs)
import Vendor.FilePath (normaliseEx)


data CatOptions = CatOptions { queryPath :: String } deriving (Show)

catParamsParser :: Parser CatOptions
catParamsParser = CatOptions
      <$> argument str
          ( metavar "PATH"
         <> help "Read file by the PATH" )


catCli :: ParserInfo CatOptions
catCli = info (catParamsParser <**> helper) ( fullDesc
            <> progDesc "Read file by the PATH"
            <> header "cat - utility to print data from file" )


catExecutor :: CatOptions -> SubprogramEnv (Maybe String)
catExecutor options = do
    let (pathToFileQ, fileName) = splitFileName $ normaliseEx $ queryPath options
    pathToFile <- getNewLocalPath pathToFileQ
    folder <- getDirectory pathToFile
    let file = Data.HashMap.lookup fileName $ getChildrens folder
    return (file >>= \f -> return $ show $ fileContent f)


cat :: Subprogram
cat = combineProgram catExecutor catCli
