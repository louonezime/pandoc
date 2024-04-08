{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Demo
-}

import Control.Applicative (Alternative((<|>), empty))
import Data.Maybe(isNothing)

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
    Right (res, rest) -> case fct2 rest of
        Right (res1, rest) -> Right((res, res1), rest)
        Left err -> Left err
    Left err -> Left err

-- parseAnd :: Parser a -> Parser b -> Parser (a,b)
-- parseAnd fct1 fct2 str =


-- CASE OF

-- parseAnd :: Parser a -> Parser b -> Parser (a,b)
-- parseAnd fct1 fct2 str = case fct1 str of
--     Right (res, rest) -> case fct2 rest of
--         Right (res1, rest) -> Right((res, res1), rest)
--         Left err -> Left err
--     Left err -> Left err

-- BIND + LAMBDA

-- parseAnd :: Parser a -> Parser b -> Parser (a,b)
-- parseAnd fct1 fct2 str = fct1 str >>= (\(res, rest) -> fct2 rest >>= (\(res1, rest) -> Right((res, res1), rest)))

-- FUNCTOR

-- parseAnd :: Parser a -> Parser b -> Parser (a,b)
-- -- new parser with runparser fc in parser type
-- <*>
-- <$>
-- parserAnd fct1 fct2 = (,) <$> fct1 <*> fct2

-- parseAnd fct1 fct1 =