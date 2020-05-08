module Programs.List
       ( list
       ) where

import qualified Data.HashMap as HM
import Data.List (intersperse, sort)
import Data.Semigroup ((<>))
import Options.Applicative

import Typings (FileSystem (..), Subprogram, SubprogramEnv)
import Utils (combineProgram, getDefault, getDirectory, getNewLocalPath)


data ListOptions = ListOptions { queryPath :: Maybe String, printByLine :: Bool } deriving (Show)


listParamsParser :: Parser ListOptions
listParamsParser = ListOptions
    <$> optional ( argument str
        ( metavar "PATH"
        <> help "Show content of directory by PATH" ))
    <*> switch
        ( long "lines"
        <> short 'l'
        <> help "Print directories by lines" )


listCli :: ParserInfo ListOptions
listCli = info (listParamsParser <**> helper) ( fullDesc
    <> progDesc "Show content of directory by PATH"
    <> header "ls - utiliry for showing subdirectories and files in directory" )


listExecutor :: ListOptions -> SubprogramEnv (Maybe String)
listExecutor options = do
    queryPathQ <- getNewLocalPath $ getDefault (queryPath options) "."
    queryFS <- getDirectory queryPathQ
    let delimiter = if printByLine options then "\n" else "\t"
    return $ Just $ concat $ intersperse delimiter $ sort $ HM.keys $ getChildrens queryFS


list :: Subprogram
list = combineProgram listExecutor listCli
