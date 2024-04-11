{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Parser
-}

module Parsing
    ( parseChar
    , parseAnyChar
    , parseSomeChar
    , parseOr
    , parseAnd
    , parseAndWith
    , parseMany
    , parseSome
    , parseUInt
    , parseInt
    , parseTuple
    , parseQuotes
    , parseSeparators
    , Parser(..)
    , connect) where

import Control.Applicative (Alternative (..))

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap fct (Parser p1) = Parser $ \str -> case p1 str of
        Right (x, str2) -> Right (fct x, str2)
        Left err -> Left err

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
        (x:xs) | c == x -> Right (c, xs)
        _ -> Left (c: ": not found")

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser $ \s ->
    case s of
        (x:xs) | x `elem` str -> Right (x, xs)
        _ -> Left (str ++ ": not found")

parseSomeChar :: String -> Parser Char
parseSomeChar = foldr (\c -> (<|>) (parseChar c)) empty

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser $ \str ->
    case runParser p1 str of
        Right (x, xs) -> Right (x, xs)
        Left _ -> runParser p2 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \str ->
    case runParser p1 str of
        Right (x, xs) ->
            case runParser p2 xs of
                Right (y, ys) -> Right ((x, y), ys)
                Left err -> Left err
        Left errb -> Left errb

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \str ->
    case runParser p1 str of
        Right (x, xs) ->
            case runParser p2 xs of
                Right (y, ys) -> Right (f x y, ys)
                Left err -> Left err
        Left errb -> Left errb

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \str ->
    case runParser p str of
        Right (x, xs) ->
            case runParser (parseMany p) xs of
                Right (y, ys) -> Right (x:y, ys)
                Left _ -> Right ([x], xs)
        Left _ -> Right ([], str)

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar "0123456789")

parseInt :: Parser Int
parseInt =
  fmap read (parseNumbers <|> connect <$> parseChar '-' <*> parseNumbers)

parseNumbers :: Parser String
parseNumbers = some (parseSomeChar ['0'..'9'])

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = parseChar '(' *> parseAnd p (parseChar ',')
    >>= \(x, _) -> parseAnd p (parseChar ')') >>= \(y, _) -> return (x, y)

parseQuotes :: Parser String
parseQuotes = parseChar '\"' *> parseSome (parseNonStr "\"") <* parseChar '\"'

parseNonStr :: String -> Parser Char
parseNonStr str = Parser $ \s ->
    case s of
        (x:xs) | x `notElem` str -> Right (x, xs)
        _ -> Left (str ++ ": found")

parseSeparators :: Parser String
parseSeparators = parseSome (parseAnyChar " \t\n")

connect :: a -> [a] -> [a]
connect x xs = x:xs
