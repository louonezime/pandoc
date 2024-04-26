{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Parser
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Parsing (
    parseChar,
    parseAnyChar,
    parseSomeChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseTuple,
    parseQuotes,
    parseSeparators,
    parseNonStr,
    parseString,
    parseAfter,
    parseBetween,
    parseBetweenTwo,
    parseBefore,
    parseCharInStr,
    parseLine,
    parseTillEmpty,
    parseString,
    parseCharInStr,
    Parser (..),
) where

import Control.Applicative (Alternative (..))
import Control.Monad ((>=>))
import Data.List (isPrefixOf)

newtype Parser a = Parser
    { runParser :: String -> Either String (a, String)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \str -> fmap (\(x, s) -> (f x, s)) (p str)

instance Applicative Parser where
    pure x = Parser $ \str -> Right (x, str)
    (Parser p1) <*> (Parser p2) = Parser $ \str -> case p1 str of
        Right (fct, xs) -> case p2 xs of
            Right (x, ys) -> Right (fct x, ys)
            Left err -> Left err
        Left errb -> Left errb

instance Alternative Parser where
    empty = Parser $ \_ -> Left "empty"
    p1 <|> p2 = Parser $ \str -> case runParser p1 str of
        Right (x, xs) -> Right (x, xs)
        Left _ -> runParser p2 str

instance Monad Parser where
    return = pure
    p >>= fct = Parser $ \str -> case runParser p str of
        Right (x, xs) -> runParser (fct x) xs
        Left err -> Left err

parseChar :: Char -> Parser Char
parseChar c = Parser $ \str ->
    case str of
        (x : xs) | c == x -> Right (c, xs)
        _ -> Left (c : ": not found")

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser $ \s ->
    case s of
        (x : xs) | x `elem` str -> Right (x, xs)
        _ -> Left (str ++ ": not found")

parseSomeChar :: String -> Parser Char
parseSomeChar = foldr ((<|>) . parseChar) empty

parseString :: String -> Parser String
parseString str = parseMany (parseSomeChar str)

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = p1 <|> p2

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = (,) <$> p1 <*> p2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = f <$> p1 <*> p2

parseMany :: Parser a -> Parser [a]
parseMany p = many p <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar "0123456789")

parseInt :: Parser Int
parseInt =
    fmap read (parseNumbers <|> (:) <$> parseChar '-' <*> parseNumbers)

parseNumbers :: Parser String
parseNumbers = some (parseSomeChar ['0' .. '9'])

parseTuple :: Parser a -> Parser (a, a)
parseTuple p =
    (,) <$> (parseChar '(' *> p) <*> (parseChar ',' *> p) <* parseChar ')'

parseQuotes :: Parser String
parseQuotes = parseChar '\"' *> parseSome (parseNonStr "\"") <* parseChar '\"'

parseLine :: Parser String
parseLine = parseSome (parseNonStr "\n") <* parseChar '\n'

parseNonStr :: String -> Parser Char
parseNonStr str = Parser $ \s ->
    case s of
        [] -> Left "No more thing to parse"
        (x : xs) | x `notElem` str -> Right (x, xs)
        _ -> Left (str ++ ": found")

parseCharInStr :: Char -> Parser Char
parseCharInStr c = Parser $ \str ->
    case str of
        (x : xs) -> if c == x
                    then Right (x, xs)
                    else runParser (parseCharInStr c) xs
        _ -> Left (c : ": not found in string")

parseSeparators :: Parser String
parseSeparators = parseSome (parseAnyChar " \t\n")

parseAfter :: String -> Parser String
parseAfter str = Parser $ \s ->
    case str `isPrefixOf` s of
        True -> Right (str, drop (length str) s)
        False -> Left (str ++ ": not prefix")

parseBefore :: String -> Parser String
parseBefore str = Parser $ \s ->
    case subStrIdx s str 0 of
        -1 -> Left (str ++ ": not a suffix")
        n -> Right (take n s, drop (n + length str) s)

subStrIdx :: String -> String -> Int -> Int
subStrIdx [] _ _ = -1
subStrIdx _ [] _ = -1
subStrIdx s target n
    | take (length target) s == target = n
    | otherwise = subStrIdx (tail s) target (n + 1)

parseBetween :: String -> Parser String
parseBetween start = parseAfter start >>= parseBefore

parseBetweenTwo :: String -> String -> Parser String
parseBetweenTwo start end = parseAfter start >>= \_ ->
    parseBefore end >>= return

parseCharInStr :: Char -> Parser Char
parseCharInStr c = Parser $ \str ->
    case str of
        (x : xs) -> if c == x
                    then Right (x, xs)
                    else runParser (parseCharInStr c) xs
        _ -> Left (c : ": not found in string")

parseTillEmpty :: Parser a -> Parser [a]
parseTillEmpty p = Parser $ \str ->
    case runParser p str of
        Right (x, []) -> Right ([x], [])
        Right (x, xs) ->
            case runParser (parseMany p) xs of
                Right (y, ys) -> Right (x : y, ys)
                Left _ -> Right ([x], xs)
        Left _ -> Right ([], str)
