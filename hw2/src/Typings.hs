{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Typings
       ( ApplicationContext(..)
       , ApplicationState(..)
       , CDException(..)
       , SubprogramEnv
       , Subprogram
       ) where

import Data.IORef
import Control.Exception (Exception)
import Data.Dynamic (Typeable)
import Control.Monad.State (State)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)

data ApplicationContext = ApplicationContext { getRootPath :: FilePath
                , currentPath                              :: IORef String
                , storedValue                              :: IORef Int }


data ApplicationState = ApplicationState { currentStatePath :: String } deriving (Show)


data CDException = SubprogramException String deriving (Show, Typeable, Exception)


type SubprogramEnv = ReaderT ApplicationContext (ExceptT CDException (State ApplicationState))


type Subprogram = [String] -> SubprogramEnv String

