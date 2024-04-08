import Control.Applicative (Alternative((<|>), empty))
import Data.Maybe(isNothing)

-- MAYBE

-- type Parser a = String -> Maybe (a, String)

-- parseChar :: Char -> Parser Char
-- parseChar _ [] = Nothing
-- parseChar c (x:xs) | c == x = Just (c, xs)
                --    | otherwise = Nothing

-- EITHER

type Parser a = String -> Either String (a, String)

parseChar :: Char -> Parser Char
parseChar _ [] = Left "empty string"
parseChar c (x:xs) | c == x = Right (x, xs)
                   | otherwise = Left (c: "not found")

-- parseAnyChar :: String -> Parser Char
-- parseAnyChar [] _ = Left "empty string"
-- parseAnyChar _ [] = Left "empty string"
-- parseAnyChar (h:t) (x:xs) | h == x = Right (x, xs)
--                           | otherwise = parseAnyChar t xs

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Left "empty string"
parseAnyChar _ [] = Left "empty string"
parseAnyChar str (h:t) | True `elem` find = Right(h,t)
    | otherwise = Left (str ++ " not found")
    where find = map (==h) str

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

parseAnd :: Parser a -> Parser b -> Parser (a,b)
-- new parser with runparser fc in parser type
<*>
<$>
parserAnd fct1 fct2 = (,) <$> fct1 <*> fct2

parseAnd fct1 fct1 =