module Lib
    ( someFunc
    , main1
    ) where

import Debug.Trace
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Applicative

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"


someFunc :: Int -> Int
someFunc 0 = 1
someFunc n = let x = someFunc (n - 1) in traceShow x $ n * x



emailIsValid :: String -> Bool
emailIsValid email = '@' `elem` email

askEmail :: MaybeT IO String
askEmail = do
    lift $ putStrLn "Input your email, please:"
    email <- lift getLine
    guard $ emailIsValid email
    return email

main1 :: IO ()
main1 = do
    Just email <- runMaybeT $ untilSuccess askEmail
    putStrLn $ "OK, your email is " ++ email

untilSuccess :: Alternative f => f a -> f a
untilSuccess = foldr (<|>) empty . repeat



-- doesPathExist :: FilePath -> Reader Env

-- changeDirectoryOnSuccessful :: FilePath -> FilePath -> Either String FilePath
-- changeDirectoryOnSuccessful a b = let x = a </> b in if doesPathExist x

