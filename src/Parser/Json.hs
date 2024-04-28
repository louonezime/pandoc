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
    (parseJsonString <|> parseJsonArray <|> parseJsonSection <|>
    parseJsonFormat "\"bold\"" <|>
    parseJsonFormat "\"italic\"" <|> parseJsonFormat "\"code\"" <|> 
    parseJsonList <|> parseJsonCodeBlock <|> parseJsonParagraph
    <|> parseJsonImage <|> parseJsonLink) <*
    optional (parseSeparators) <* optional (parseChar ','))

parseJsonEntries :: Parser [Entry]
parseJsonEntries = parseTillEmpty parseJsonEntry

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
        Right (x, str) -> Right (parseJsonHeaderTags header str, 
            parseJsonHeaderContent str)
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

parseJsonHeaderContent :: String -> String
parseJsonHeaderContent s = case runParser (optional (parseSeparators)
    *> parseChar '{' *>
    parseJsonObjectContent <* parseChar '}') s of
        Right (x, xs) -> xs
        Left e -> ":"

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
parseJsonFormatObject s = Parser $ \_ -> Left ("Invalid format field " ++ s)

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
            (parseJsonFieldString "\"title\"") str of
                Right (title, str2) -> runParser
                    (parseJsonSectionContent title) str2
                Left e -> Left e
        Left e -> Left e

parseJsonFieldString :: String -> Parser Entry
parseJsonFieldString key = parseJsonTag key
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonString
    >>= \(y) -> return y

parseJsonField :: String -> Parser Entry
parseJsonField field = (parseChar ',') *> parseJsonTag field
    >>= \(x, _) -> optional (parseSeparators) *> parseJsonEntry <*
    optional (parseSeparators) >>= \(y) -> return y

parseJsonSectionContent :: Entry -> Parser Entry
parseJsonSectionContent title = Parser $ \str -> case runParser
    (parseJsonField "\"content\"") str of
        Right (contentValue, str2) -> Right ((Section {
            sectionTitle = txt title,
            content = contentValue:[] }), str2)
        Left e -> Left e

parseJsonCodeBlock :: Parser Entry
parseJsonCodeBlock = Parser $ \s -> case runParser
    (parseJsonObjectKey "\"codeblock\"") s of
        Right (_, str) -> case runParser (parseJsonArray) str of
            Right (res, str2) -> Right (CodeBlock { 
                content = res:[] }, str2)
            Left e -> Left e
        Left e -> Left e

parseJsonList :: Parser Entry
parseJsonList = Parser $ \s -> case runParser
    (parseJsonObjectKey "\"list\"") s of
        Right (_, str) -> case runParser (parseJsonArray) str of
            Right (res, str2) -> Right (List { 
                content = res:[] }, str2)
            Left e -> Left e
        Left e -> Left e

parseJsonParagraph :: Parser Entry
parseJsonParagraph = Parser $ \s -> case runParser
    (parseJsonObjectKey "\"paragraph\"") s of
        Right (_, str) -> case runParser (parseJsonArray) str of
            Right (res, str2) -> Right (Paragraph { 
                content = res:[] }, str2)
            Left e -> Left e
        Left e -> Left e


parseJsonObjectKey :: String -> Parser (String, Char)
parseJsonObjectKey key = (optional (parseSeparators) *> 
    parseChar '{' *> optional (parseSeparators) *>
    parseAnd (parseAfter key <* optional (parseSeparators))
    (parseChar ':'))
    
parseJsonImage :: Parser Entry
parseJsonImage = Parser $ \s -> case runParser
    (parseJsonObjectKey "\"image\"") s of
        Right (_, str) -> case runParser
            (parseJsonFieldString "\"url\"") str of
                Right (url, str2) -> runParser
                    (parseJsonImageAlt url) str2
                Left e -> Left e
        Left e -> Left e


parseJsonLink :: Parser Entry
parseJsonLink = Parser $ \s -> case runParser
    (parseJsonObjectKey "\"link\"") s of
        Right (_, str) -> case runParser
            (parseJsonFieldString "\"url\"") str of
                Right (url, str2) -> runParser
                    (parseJsonLinkContent url) str2
                Left e -> Left e
        Left e -> Left e

parseJsonLinkContent :: Entry -> Parser Entry
parseJsonLinkContent urlValue = Parser $ \str -> case runParser
    (parseJsonField "\"content\"") str of
        Right (altValue, str2) -> Right (Link { url = txt urlValue,
        alt = altValue }, str2)
        Left e -> Left e

parseJsonImageAlt :: Entry -> Parser Entry
parseJsonImageAlt urlValue = Parser $ \str -> case runParser
    (parseJsonField "\"alt\"") str of
        Right (altValue, str2) -> Right (Image { url = txt urlValue,
        alt = altValue }, str2)
        Left e -> Left e

parseJson :: Parser Document
parseJson = Parser $ \str ->
    case runParser (parseJsonHeader defaultHeader) str of
        Right (header, xs) -> case runParser parseJsonBody xs of
            Right (_, ys) -> case runParser parseJsonEntries ys of
                Right (content, zs) -> Right
                    (Document header content, [])
                Left e -> Left e
            Left e -> Left "Invalid JSON, no body found"
        Left e -> Left "Invalid JSON, no header found"

parseJsonBody :: Parser (String, Char)
parseJsonBody = (optional (parseSeparators) *> parseChar ',' 
    *> optional (parseSeparators) *>
    parseAnd (parseAfter "\"body\""
    <* optional (parseSeparators)) (parseChar ':' *> 
    optional (parseSeparators) *> parseChar '['))