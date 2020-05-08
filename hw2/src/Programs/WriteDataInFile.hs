module Programs.WriteDataInFile
       ( writeDataInFile
       ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap as HM
import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath

import Typings (ApplicationContext (..), ApplicationState (..), FileSystem (..), Subprogram,
                SubprogramEnv, SubprogramException (..))
import Utils (combineProgram, getDirectory, getNewLocalPath, replaceFs, throwIf)
import Vendor.FilePath (normaliseEx)


data WriteDataInFileOptions = WriteDataInFileOptions { textData :: String, changeDirectoryTo :: String } deriving (Show)

writeDataInFileParamsParser :: Parser WriteDataInFileOptions
writeDataInFileParamsParser = WriteDataInFileOptions
    <$> argument str
        ( metavar "TEXT"
        <> help "Write TEXT into file" )
    <*> strOption
        ( long "to"
        <> metavar "TARGET"
        <> help "Write text into file by this TARGET" )


writeDataInFileCli :: ParserInfo WriteDataInFileOptions
writeDataInFileCli = info (writeDataInFileParamsParser <**> helper) ( fullDesc
    <> progDesc "Write text into file"
    <> header "wf - utility to text into file" )


writeDataInFileExecutor :: WriteDataInFileOptions -> SubprogramEnv (Maybe String)
writeDataInFileExecutor options = do
    appState <- get
    rootPath <- asks getRootPath
    let (pathToFileQ, name) = splitFileName $ dropTrailingPathSeparator $ normaliseEx $ changeDirectoryTo options
    throwIf (not (isValid name)) $ SubprogramRuntimeException "Запрещенное название пути"
    pathToFile <- getNewLocalPath pathToFileQ
    folder <- getDirectory pathToFile
    let newFolder = folder{getChildrens = HM.insert name (File (normaliseEx $ rootPath </> pathToFile </> name) (C.pack $ textData options)) (getChildrens folder)}
    newFS <- replaceFs pathToFile newFolder
    lift $ put $ appState {getCurrentFileSystem = newFS}
    return Nothing


writeDataInFile :: Subprogram
writeDataInFile = combineProgram writeDataInFileExecutor writeDataInFileCli
