module Programs.ChangeDirectory
       ( cdOpts
       , cdCommandWrapper2
       , changeDirectory
       ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.IORef
import Data.List
import Data.Semigroup ((<>))
import Lib
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath

import Typings (ApplicationContext (..), ApplicationState (..), CDException (..), SubprogramEnv, Subprogram)
import Vendor.FilePath (normaliseEx)
import Utils (parserPrefs)


data ChangeDirectoryOptions = ChangeDirectoryOptions
  { changeDirectoryTo :: String } deriving (Show)

changeDirectoryParser :: Parser ChangeDirectoryOptions
changeDirectoryParser = ChangeDirectoryOptions
      <$> argument str
          ( metavar "DIRECTORY"
         <> help "Change directory to the DIRECTORY" )


cdOpts :: ParserInfo ChangeDirectoryOptions
cdOpts = info (changeDirectoryParser <**> helper) ( fullDesc
            <> progDesc "Change directory to the TARGET"
            <> header "cd - a test for optparse-applicative" )


-- greet :: ParserResult ChangeDirectoryOptions -> IO ()
-- greet (Success (ChangeDirectoryOptions h)) = putStrLn $ "Hello, " ++ h
-- greet _ = return ()

-- cdCommandWrapper :: ParserResult ChangeDirectoryOptions -> IO ()
-- cdCommandWrapper (Success a) = putStrLn $ show a
-- cdCommandWrapper f = putStrLn $ show f


cdCommandWrapper2 :: ParserResult ChangeDirectoryOptions -> SubprogramEnv String
cdCommandWrapper2 (Success a) = do
    cdCommand a
    return ""
cdCommandWrapper2 f = throwError $ SubprogramException $ show f

-- cdCommand :: ChangeDirectoryOptions -> State ApplicationState ()
cdCommand :: ChangeDirectoryOptions -> SubprogramEnv ()
cdCommand options = do
    state <- get
    rootPath <- asks getRootPath
    let futurePath = normaliseEx $ currentStatePath state </> changeDirectoryTo options
    let diff = makeRelative rootPath futurePath
    if
        not (isRelative diff) || (isPrefixOf (".." ++ [pathSeparator]) diff) || (".." == diff)
    then
        throwError $ SubprogramException "Нельзя покинуть рабочую директорию"
    else
        lift $ put $ ApplicationState $ normaliseEx $ currentStatePath state </> changeDirectoryTo options



changeDirectory :: Subprogram
changeDirectory params = do
    let params2 = execParserPure parserPrefs cdOpts params
    cdCommandWrapper2 params2

-- main :: IO ()
-- main = do
--     args <- getArgs
--     let params = execParserPure parserPrefs opts args
--     -- params <- execParser opts
--     greet params

