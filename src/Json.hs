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
import Data.List

parseJsonEntry :: Parser Entry
parseJsonEntry = (optional (parseSeparators) *>
    (parseJsonString <|> parseJsonArray <|> parseJsonObject) <*
    optional (parseSeparators) <* optional (parseChar ','))

-- parseJsonEntries :: Parser [Entry]
-- parseJsonEntries = Parser $ \s -> case runParser
--     (parseSome parseJsonEntry) s of
--         Right (x, str) -> Right (x, str) 
--         Left e -> Left e

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
    parseKeyValue <* optional (parseSeparators) <* optional (parseChar ','))
    <* optional (parseSeparators))

parseKeyValue :: Parser (String, [Entry])
parseKeyValue = optional (parseSeparators) *>
    parseAnd parseQuotes (parseChar ':') <* optional (parseSeparators)
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonEntry
    >>= \(y) -> return (x, y:[])

defaultHeader :: Header
defaultHeader = Header
    { title = "",
      author = Nothing,
      date = Nothing
    }

parseJsonHeader :: Header -> Parser Header
parseJsonHeader header = Parser $ \s -> case runParser
    (parseJsonHeaderKey header) s of
        Right (x, str) -> Right (parseJsonHeaderTags header str, str)
        Left e -> Left e

parseJsonHeaderKey :: Header -> Parser String
parseJsonHeaderKey header = parseSome (parseChar '{' *>
    (optional (parseSeparators) *>
    parseAfter "\"header\"" <* optional (parseSeparators))
    <* optional (parseSeparators) *> (parseChar ':'))

parseJsonHeaderTags :: Header -> String -> Header
parseJsonHeaderTags header "" = header
parseJsonHeaderTags header str = case runParser (parseJsonDateTag header
    <|> parseJsonAuthorTag header <|> parseJsonTitleTag header) str of
        Right (x, xs) -> parseJsonHeaderTags x xs
        _ -> header

parseJsonTag :: String -> Parser (String, Char)
parseJsonTag tag = optional (parseSeparators) *>
    optional (parseChar '{') *> optional (parseSeparators) *> 
    parseAnd (parseAfter tag) (parseChar ':')
    <* optional (parseSeparators)

parseJsonDateTag :: Header -> Parser Header 
parseJsonDateTag header = parseJsonTag "\"date\""
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonEntry
    >>= \(y) -> return header { date = Just (txt y) }

parseJsonAuthorTag :: Header -> Parser Header 
parseJsonAuthorTag header = parseJsonTag "\"author\""
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonEntry
    >>= \(y) -> return header { author = Just (txt y) }

parseJsonTitleTag :: Header -> Parser Header 
parseJsonTitleTag header = parseJsonTag "\"title\""
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonEntry
    >>= \(y) -> return header { title = txt y }