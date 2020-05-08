module Utils
       ( parserPrefs
       , combineProgram
       ) where

import Options.Applicative

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.IORef
import Data.List
import Data.Semigroup ((<>))
import Lib
import System.Directory
import System.Environment
import System.FilePath


import Typings (ApplicationContext (..), ApplicationState (..), SubprogramException (..), SubprogramEnv, Subprogram)
import Vendor.FilePath (normaliseEx)

parserPrefs :: ParserPrefs
parserPrefs = ParserPrefs "" False False False False 1



combineProgram :: (Show a) => (a -> SubprogramEnv (Maybe String)) -> ParserInfo a -> Subprogram
combineProgram program parser = \args -> do
    let params = execParserPure parserPrefs parser args
    cdCommandWrapper2 program params


cdCommandWrapper2 :: (Show a) => (a -> SubprogramEnv (Maybe String)) -> ParserResult a -> SubprogramEnv (Maybe String)
cdCommandWrapper2 program (Success a) = do
    program a
    return $ Nothing
cdCommandWrapper2 program f = throwError $ SubprogramArgumentsException $ show f
