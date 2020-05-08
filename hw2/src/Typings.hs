{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Typings
       ( ApplicationContext(..)
       , FileSystem(..)
       , ApplicationState(..)
       , SubprogramException(..)
       , SubprogramEnv
       , Subprogram
       ) where

import Control.Exception (Exception)
import Control.Monad.State (State)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Dynamic (Typeable)
import Data.IORef
import Data.HashMap

data FileSystem = Directory { directoryName :: String, getChildrens :: Map String FileSystem } | File { fileName :: String } | Stub deriving (Show)


data ApplicationContext = ApplicationContext { getRootPath          :: FilePath
                                             , getInitialFileSystem :: FileSystem }


data ApplicationState = ApplicationState { getCurrentStatePath :: String, getCurrentFileSystem :: FileSystem} deriving (Show)


data SubprogramException = SubprogramArgumentsException String | SubprogramRuntimeException String deriving (Show, Typeable, Exception)


type SubprogramEnv = ReaderT ApplicationContext (ExceptT SubprogramException (State ApplicationState))


type Subprogram = [String] -> SubprogramEnv (Maybe String)

