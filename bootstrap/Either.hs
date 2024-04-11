{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Demo
-}

import Control.Applicative (Alternative((<|>), empty))
import Data.Maybe(isNothing)
import Text.Read (readMaybe)
import Data.Char (isDigit, isSpace)

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

parseOr :: Parser a -> Parser a -> Parser a
parseOr fct1 fct2 str = case fct1 str of
    Right res -> Right res
    Left _ -> fct2 str

-- parseAnyCharAlias :: String -> Parser Char

-- Step 1.2.2

parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd fct1 fct2 str =
    fct1 str >>= (\(a, b) -> fct2 b >>= (\(c, b) -> Right((a, c), b)))

--equivalent of

parseAndA :: Parser a -> Parser b -> Parser (a,b)
parseAndA fct1 fct2 str = case fct1 str of
    Right (a, b) -> case fct2 b of
        Right (c, b) -> Right((a, c), b)
        Left err -> Left err
    Left err -> Left err

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

parseUInt :: Parser Int -- parse an unsigned Int
parseUInt "" = Left "no unsigned int"
parseUInt str = case span isDigit str of
    ("", _) -> Left "no digit"
    (digits, rest) -> Right (read digits, rest)

parseInt :: Parser Int -- parse an signed Int
parseInt "" = Left "no signed int"
parseInt ('+':rest) = parseUInt rest
parseInt ('-':rest) = case parseUInt rest of
    Right (n, rest') -> Right ((-n), rest')
    Left err -> Left err
parseInt str = parseUInt str

parseTuple :: Parser a -> Parser (a, a)
parseTuple pa str = do
    let trimmed = dropWhile isSpace str
    case trimmed of
        ('(':rest1) -> do
            (val1, rest2) <- pa (dropWhile isSpace rest1)
            case rest2 of
                (',':rest3) -> do
                    (val2, rest4) <- pa (dropWhile isSpace rest3)
                    case rest4 of
                        (')':rest5) -> Right ((val1, val2), rest5)
                        _ -> Left "expected closing parenthesis ')'"
                _ -> Left "expected comma ','"
        _ -> Left "expected opening parenthesis '('"

type ParserI a = String -> Either String Int

parseUIntA :: ParserI Int -- parse an unsigned Int
parseUIntA [] = Left "no unsigned int"
parseUIntA (x:xs) = case readMaybe (x:xs) of
    Just a -> if a < 0 then (Left "no unsigned int") else (Right a)
    Nothing -> Left "no unsigned int"

parseIntA :: ParserI Int -- parse an signed Int
parseIntA [] = Left "no signed int"
parseIntA (x:xs) = case readMaybe (x:xs) of
    Just a -> Right a
    Nothing -> Left "no unsigned int"

-- Step 2.1 (Parser.hs)

-- Notes from Demo:

-- FUNCTOR
-- parseAnd :: Parser a -> Parser b -> Parser (a,b)
-- -- new parser with runparser fc in parser type
-- <*>
-- <$>
-- parserAnd fct1 fct2 = (,) <$> fct1 <*> fct2

-- parseAnd fct1 fct1 =
