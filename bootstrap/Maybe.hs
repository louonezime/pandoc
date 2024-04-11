--
-- EPITECH PROJECT, 2024
-- bootstrap
-- File description:
-- Maybe
--

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
