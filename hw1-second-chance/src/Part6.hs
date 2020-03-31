{-# LANGUAGE InstanceSigs #-}

module Part6
       ( Parser(..)
       , ok
       , eof
       , satisfy
       , element
       , stream

       , parseBBS
       , parseInteger

       , listlistParser
       ) where

import Control.Monad
import Control.Applicative
import Data.Char


-- task 1
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f p = Parser $ \w -> (\(x, y) -> (f x, y)) <$> (runParser p) w

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser $ \s -> Just (a, s)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    (<*>) = ap

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ \_ -> Nothing

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    (<|>) (Parser p1) (Parser p2) = Parser $ \s -> (p1 s) <|> (p2 s)


instance Monad (Parser s) where
    return :: a -> Parser s a
    return value = Parser $ \s -> Just (value, s)

    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    (>>=) (Parser p) f = Parser $ \s -> case p s of
        Just (a, x) -> runParser (f a) x
        _           -> Nothing


-- task2
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
    [] -> Just ((), [])
    _ -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy cond = Parser $ \s -> case s of
    (x : xs) -> if cond x then Just (x, xs) else Nothing
    [] -> Nothing

element :: Eq s => s -> Parser s s
element c = satisfy (== c)

stream :: Eq s => [s] -> Parser s [s]
stream = traverse element


-- task3
parseBBSInner :: Parser Char String
parseBBSInner = 
    (
        do
            s1 <- element '('
            inner <- parseBBSInner
            s2 <- element ')'
            outer <- parseBBSInner
            return $ [s1] ++ inner ++ [s2] ++ outer
    ) <|> (
        do
            return ""
    )

parseBBS :: Parser Char String
parseBBS = do
    bbs <- parseBBSInner
    eof
    return $ bbs


parseNumber :: Parser Char Int
parseNumber = do
    signChar <- (stream "+") <|> (stream "-") <|> (stream "")
    number <- some (satisfy isDigit)
    let sign = (if signChar == "-" then -1 else 1)
    return $ sign * foldl (\acc x -> acc * 10 + digitToInt x) 0 number

parseInteger :: Parser Char Int
parseInteger = do
    num <- parseNumber
    eof
    return num


-- task4
skipWhitespaces :: Parser Char ()
skipWhitespaces = (many $ satisfy isSpace) >> ok

listParser :: Int -> Parser Char [Int]
listParser 0 = pure []
listParser n = do
    skipWhitespaces
    _ <- element ','
    skipWhitespaces
    num  <- parseNumber
    rest <- listParser (n - 1)
    return $ num : rest


listlistParserRest :: Parser Char [[Int]]
listlistParserRest =
    (
        do
            skipWhitespaces
            _ <- element ','
            skipWhitespaces
            listLen <- parseNumber
            list <- listParser listLen
            rest <- listlistParserRest
            return $ list : rest
    ) <|> (
        do
            skipWhitespaces
            eof
            return []
    )


listlistParser :: Parser Char [[Int]]
listlistParser = do
    skipWhitespaces
    firstLen <- parseNumber
    list <- listParser firstLen
    rest <- listlistParserRest
    return $ list : rest
