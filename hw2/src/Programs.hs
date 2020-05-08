{-# LANGUAGE OverloadedStrings #-}

module Programs (getProgram) where

import Typings (FileSystem(..), ApplicationContext(..), ApplicationState(..), SubprogramException(..), Subprogram)
import Programs.ChangeDirectory (changeDirectory)
import Programs.List (list)
import Programs.MakeDirectory (makeDirectory)
import Vendor.FilePath (normaliseEx)
-- import Shit (getFileSystem)

-- import Control.Monad.State
-- -- import Control.Monad.Reader
-- import Control.Monad.Trans
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.Except
-- -- import Control.Monad.Trans.Maybe
-- -- import Control.Applicative
-- import Data.IORef
-- import System.Environment
-- import System.Directory
-- import System.FilePath
-- import Options.Applicative
-- import Data.Semigroup ((<>))
-- import Lib
-- -- import Control.Monad.Error
-- import Control.Monad.Except
-- import Data.List
-- import System.IO

-- import Debug.Trace


defaultProgram :: String -> Subprogram
defaultProgram name _ = return $ Just $ name ++ " not found"


getProgram :: String -> Subprogram
getProgram x = case x of
            "cd" -> changeDirectory
            "ls" -> list
            "mkdir" -> makeDirectory
            _ -> defaultProgram x

