{-# LANGUAGE OverloadedStrings #-}

module Shit (getFileSystem) where

import Typings (ApplicationContext(..), ApplicationState(..), CDException(..), Subprogram)
import Programs.ChangeDirectory (cdOpts, cdCommandWrapper2, changeDirectory)
import Vendor.FilePath (normaliseEx)


import Control.Monad.State
-- import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Maybe
-- import Control.Applicative
import Data.IORef
import System.Environment
import System.Directory
import System.FilePath
import Options.Applicative
import Data.Semigroup ((<>))
import Lib
import Control.Monad.Error
import Data.List
import System.IO

import Debug.Trace






-- data Metainfo = Metainfo { editDate :: String }

data FileSystem = Directory { directoryName :: String, getChildrens :: [FileSystem] } | File { fileName :: String } | Stub deriving (Show)

getFileSystem :: FilePath -> IO FileSystem
getFileSystem = undefined
-- getFileSystem path = do
--     isDirectory <- doesDirectoryExist path
--     if isDirectory then do
--         childrens <- listDirectory path
--         rez <- mapM (getFileSystem . (path </>)) childrens
--         return $ Directory path rez
--     else if pathIsSymbolicLink path then do
--         return Stub
--     else do
--         return $ File $ takeFileName path