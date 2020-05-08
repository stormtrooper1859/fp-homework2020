module Programs.Cat
       ( cat
       ) where

import Data.HashMap
import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath

import Typings (FileSystem (..), Subprogram, SubprogramEnv, SubprogramException (..))
import Utils (combineProgram, getDirectory, getNewLocalPath, throwMaybe, throwIf, isFile)
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
    let (pathToFileQ, name) = splitFileName $ normaliseEx $ queryPath options
    pathToFile <- getNewLocalPath pathToFileQ
    folder <- getDirectory pathToFile
    let maybeFile = Data.HashMap.lookup name $ getChildrens folder
    file <- throwMaybe maybeFile $ SubprogramRuntimeException "Файла не существует"
    throwIf (not (isFile file)) $ SubprogramRuntimeException "Невозможно прочитать директорию"
    return $ Just $ show $ fileContent file


cat :: Subprogram
cat = combineProgram catExecutor catCli
