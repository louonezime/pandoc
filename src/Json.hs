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
    (parseJsonString <|> parseJsonObject <|> parseJsonArray <|>
    parseJsonFormat "\"bold\"" <|> parseJsonFormat "\"italic\"" <|>
    parseJsonFormat "\"code\"" <|> parseJsonEntryArray "\"codeblock\""
    <|> parseJsonEntryArray "\"list\"" <|>
    parseJsonEntryArray "\"paragraph\"") <*
    optional (parseSeparators) <* optional (parseChar ','))

parseJsonEntries :: Parser [Entry]
parseJsonEntries = Parser $ \s -> case runParser
    (parseSome parseJsonEntry) s of
        Right (x, str) -> Right (x, str) 
        Left e -> Left e

parseJsonString :: Parser Entry
parseJsonString = Parser $ \s -> case runParser parseQuotes s of
    Right (x, xs) -> Right (Text x, xs)
    Left e -> Left e 

parseJsonArray :: Parser Entry
parseJsonArray = Parser $ \s -> case runParser
    (optional (parseSeparators) *> parseChar '[' *>
    parseJsonArrayContent <* optional (parseSeparators) <* parseChar ']') s of
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
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonString
    >>= \(y) -> return header { date = Just (txt y) }

parseJsonAuthorTag :: Header -> Parser Header 
parseJsonAuthorTag header = parseJsonTag "\"author\""
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonString
    >>= \(y) -> return header { author = Just (txt y) }

parseJsonTitleTag :: Header -> Parser Header 
parseJsonTitleTag header = parseJsonTag "\"title\""
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonString
    >>= \(y) -> return header { title = txt y }

parseJsonFormat :: String -> Parser Entry
parseJsonFormat format = Parser $ \s -> case runParser
    (parseJsonFormatObject format) s of
        Right (x, str) -> Right (x, str)
        Left e -> Left e

parseJsonFormatObject :: String -> Parser Entry
parseJsonFormatObject "\"bold\"" = Parser $ \s -> case runParser
    (parseChar '{' *> (parseJsonFormatContent "\"bold\"")
    <* parseChar '}') s of
    Right ((key, val), str) -> Right (Bold { bold = val } , str)
    Left e -> Left e
parseJsonFormatObject "\"italic\"" = Parser $ \s -> case runParser
    (parseChar '{' *> (parseJsonFormatContent "\"italic\"")
    <* parseChar '}') s of
    Right ((key, val), str) -> Right (Italic { italic = val } , str)
    Left e -> Left e
parseJsonFormatObject "\"code\"" = Parser $ \s -> case runParser
    (parseChar '{' *> (parseJsonFormatContent "\"code\"")
    <* parseChar '}') s of
    Right ((key, val), str) -> Right (Code { code = val } , str)
    Left e -> Left e
parseJsonFormatObject _ = error "invalid format"

parseJsonFormatContent :: String -> Parser (String, Entry)
parseJsonFormatContent format = (optional (parseSeparators) *>
    (parseFormatKeyValue format) <* optional (parseSeparators))
    <* optional (parseSeparators)

parseFormatKeyValue :: String -> Parser (String, Entry)
parseFormatKeyValue format = optional (parseSeparators) *>
    parseAnd (parseAfter format) (parseChar ':') <* optional (parseSeparators)
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonString
    >>= \(y) -> return (x, y)

parseJsonSection :: Parser Entry
parseJsonSection = Parser $ \s -> case runParser
    (parseJsonObjectKey "\"section\"") s of
        Right (_, str) -> case runParser
            (parseJsonSectionTitle) str of
                Right (title, str2) -> runParser
                    (parseJsonSectionContent title) str2
                Left e -> Left e
        Left e -> Left e

parseJsonSectionTitle :: Parser Entry
parseJsonSectionTitle = parseJsonTag "\"title\""
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonString
    >>= \(y) -> return y

parseJsonSectionContentValue :: Parser Entry
parseJsonSectionContentValue = (parseChar ',') *> parseJsonTag "\"content\""
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonEntry <*
    optional (parseSeparators) >>= \(y) -> return y

parseJsonSectionContent :: Entry -> Parser Entry
parseJsonSectionContent title = Parser $ \str -> case runParser
    (parseJsonSectionContentValue) str of
        Right (content, str2) -> Right ((Section {
            sectionTitle = txt title,
            content = content:[] }), str2)
        Left e -> Left e

parseJsonEntryArray :: String -> Parser Entry
parseJsonEntryArray key = Parser $ \s -> case runParser
    (parseJsonObjectKey key) s of
        Right (_, str) -> case runParser (parseJsonArray) str of
            Right (res, str2) -> Right (res, str2)
            Left e -> Left e
        Left e -> Left e

parseJsonObjectKey :: String -> Parser (String, Char)
parseJsonObjectKey key = (optional (parseSeparators) *> 
    parseChar '{' *> optional (parseSeparators) *>
    parseAnd (parseAfter key <* optional (parseSeparators)
    <* optional (parseSeparators)) (parseChar ':'))


