{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Demo
-}

import Control.Applicative (Alternative((<|>), empty))
import Data.Maybe(isNothing)
import Text.Read

-- MAYBE
-- Step 1.1.1

type ParserA a = String -> Maybe (a, String)

parseCharA :: Char -> ParserA Char
parseCharA _ [] = Nothing
parseCharA c (x:xs) | c == x = Just (c, xs)
                    | otherwise = Nothing

-- Step 1.1.2

parseAnyCharA :: String -> ParserA Char
parseAnyCharA [] _ = Nothing
parseAnyCharA _ [] = Nothing
parseAnyCharA (h:t) (x:xs)
    | x == h = Just (x, xs)
    | otherwise = parseAnyCharA t (x:xs)

-- EITHER
-- Step 1.1.1

type Parser a = String -> Either String (a, String)

parseChar :: Char -> Parser Char
parseChar _ [] = Left "empty string"
parseChar c (x:xs) | c == x = Right (x, xs)
                   | otherwise = Left (c: ": not found")

-- Step 1.1.2

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Left "empty string"
parseAnyChar _ [] = Left "empty string"
parseAnyChar str (h:t) | True `elem` find = Right(h,t)
    | otherwise = Left (str ++ ": not found")
    where find = map (==h) str

-- Step 1.2.1

parseOrA :: ParserA a -> ParserA a -> ParserA a
parseOrA fct1 fct2 str = case fct1 str of
    Just res -> Just res
    Nothing -> fct2 str

parseOr :: Parser a -> Parser a -> Parser a
parseOr fct1 fct2 str = case fct1 str of
    Right res -> Right res
    Left _ -> fct2 str

-- parseAnyCharAlias :: String -> Parser Char

-- Step 1.2.2

parseAndA :: Parser a -> Parser b -> Parser (a,b)
parseAndA fct1 fct2 str = case fct1 str of
    Right (a, b) -> case fct2 b of
        Right (c, b) -> Right((a, c), b)
        Left err -> Left err
    Left err -> Left err

parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd fct1 fct2 str =
    fct1 str >>= (\(a, b) -> fct2 b >>= (\(c, b) -> Right((a, c), b)))

-- Step 1.2.3

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith ord fct1 fct2 str =
    fct1 str >>= (\(a, b) -> fct2 b >>= (\(c, b) -> Right(ord a c, b)))

-- Step 1.2.4

parseMany :: Parser a -> Parser [a]
parseMany fct (x:xs) = case fct (x:xs) of
    Right (a, b) -> case parseMany fct b of
        Right (c, d) -> Right (a:c, d)
        Left _ -> Right ([a], b)
    Left _ -> Right ([], x:xs)

-- Step 1.2.5

parseSome :: Parser a -> Parser [a]
parseSome fct (x:xs) = case fct (x:xs) of
    Right (a, b) -> case parseSome fct b of
        Right (c, d) -> Right (a:c, d)
        Left _ -> Right ([a], b)
    Left err -> Left err

-- Step 1.3.1

type ParserI a = String -> Either String Int

parseUInt :: ParserI Int -- parse an unsigned Int
parseUInt [] = Left "no unsigned int"
parseUInt (x:xs) = case readMaybe (x:xs) of
    Just a -> if a < 0 then (Left "no unsigned int") else (Right a)
    Nothing -> Left "no unsigned int"

parseInt :: ParserI Int -- parse an signed Int
parseInt [] = Left "no signed int"
parseInt (x:xs) = case readMaybe (x:xs) of
    Just a -> Right a
    Nothing -> Left "no unsigned int"

parseTuple :: Parser a -> Parser (a, a) -- parse a tuple
parseTuple fct str = case fct str of
    Right (a, b) -> case parseChar ',' b of
        Right (_, c) -> case fct c of
            Right (d, e) -> Right ((a, d), e)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

-- FUNCTOR
-- parseAnd :: Parser a -> Parser b -> Parser (a,b)
-- -- new parser with runparser fc in parser type
-- <*>
-- <$>
-- parserAnd fct1 fct2 = (,) <$> fct1 <*> fct2

-- parseAnd fct1 fct1 =
