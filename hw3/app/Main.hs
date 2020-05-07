module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Environment
-- import Control.Monad

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

opts :: ParserInfo Sample
opts = info (sample <**> helper) ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative" )

greet :: ParserResult Sample -> IO ()
greet (Success (Sample h False n)) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()



parserPrefs :: ParserPrefs
parserPrefs = ParserPrefs "" False False False False 1

main :: IO ()
main = do
    args <- getArgs
    let params = execParserPure parserPrefs opts args
    -- params <- execParser opts
    greet params



-- start :: String -> IO ()
-- start s = do
--     putStrLn $ "start " ++ s

-- stop :: IO ()
-- stop = do
--     putStrLn "stop"

-- opts :: Parser (IO ())
-- opts = subparser
--   ( command "start" (info (start <$> argument str idm) idm)
--  <> command "stop"  (info (pure stop) idm) )

-- main :: IO ()
-- main = join $ execParser (info opts idm)
