{--
-- EPITECH PROJECT, 2024
-- json
-- File description:
-- pandoc
--}

module Json where

import Parsing
import Document
import Control.Applicative ((<|>), optional)

parseJsonEntry :: Parser Entry
parseJsonEntry = (optional (parseSeparators) *>
    (parseJsonString <|> parseJsonArray <|> parseJsonObject) <*
    optional (parseSeparators) <* optional (parseChar ','))

parseJsonString :: Parser Entry
parseJsonString = Parser $ \s -> case runParser parseQuotes s of
    Right (x, xs) -> Right (Text x, xs)
    Left e -> Left e 

parseJsonArray :: Parser Entry
parseJsonArray = Parser $ \s -> case runParser
    (parseChar '[' *> parseJsonArrayContent <* parseChar ']') s of
    Right (x, xs) -> Right (List x, xs)
    Left e -> Left e 

parseJsonArrayContent :: Parser [Entry]
parseJsonArrayContent = parseSome ((optional (parseSeparators) *> 
    parseJsonEntry <* optional (parseSeparators) <* optional (parseChar ',')))

parseJsonObject :: Parser Entry
parseJsonObject = Parser $ \s -> case runParser
    (parseChar '{' *> parseJsonObjectContent <* parseChar '}') s of
    Right (arr, str) -> Right (List {content =
        (\(key, val) -> (Section { sectionTitle = key, content = val}))
        <$> arr
        }, str)
    Left e -> Left e

parseJsonObjectContent :: Parser [(String, [Entry])]
parseJsonObjectContent = parseSome ((optional (parseSeparators) *>
    parseKeyValue <* optional (parseSeparators) <* optional (parseChar ',')))

parseKeyValue :: Parser (String, [Entry])
parseKeyValue = optional (parseSeparators) *>
    parseAnd parseQuotes (parseChar ':') <* optional (parseSeparators)
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonEntry
    >>= \(y) -> return (x, y:[])
