{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory
import System.FilePath

import FileManager (commandLoop)
import Typings (ApplicationContext (..), ApplicationState (..))
import Utils (getDefault)
import Utils.FileSystem (getFileSystem, syncWithFS)
import Vendor.FilePath (normaliseEx)


data FileManagerOptions = FileManagerOptions { queryPath :: Maybe String } deriving (Show)


fileManagerParamsParser :: Parser FileManagerOptions
fileManagerParamsParser = FileManagerOptions
    <$> optional ( argument str
        ( metavar "PATH"
        <> help "Show content of directory by PATH" ))


fileManagerCli :: ParserInfo FileManagerOptions
fileManagerCli = info (fileManagerParamsParser <**> helper) ( fullDesc
    <> progDesc "Show content of directory by PATH"
    <> header "ls - utiliry for showing subdirectories and files in directory" )


main :: IO ()
main = do
    params <- execParser fileManagerCli
    let root = getDefault (queryPath params) "."
    currentDirectory <- getCurrentDirectory
    let realRoot = normaliseEx $ currentDirectory </> root
    fs <- getFileSystem $ realRoot
    newFileSystem <- runReaderT (commandLoop $ ApplicationState "." fs) (ApplicationContext realRoot fs)
    syncWithFS realRoot fs (getCurrentFileSystem newFileSystem)
