{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Xml
-}

module Parser.Xml (parseXml) where

import Document (Document (..), Entry (..), Header (..), defaultHeader)
import Parsing (
    parseSome,
    parseNonStr,
    parseString,
    parseBetweenTwo,
    parseAndWith,
    parseBefore,
    parseChar,
    Parser (..))

parseAttributeValue :: Parser String
parseAttributeValue = parseString "=\"" *> parseSome (parseNonStr "\"")

parseAttributeName :: String -> Parser String
parseAttributeName str
    = parseString ("<" ++ str ++ " ") *> parseSome (parseNonStr " =")

parseChevron :: Parser String
parseChevron = parseChar '<' *> parseSome (parseNonStr "</>") <* parseChar '>'

parseEndChevron :: Parser String
parseEndChevron =
    parseString "</" *> parseSome (parseNonStr "<>") <* parseChar '>'

parseHeader :: Header -> Parser Header
parseHeader hdr = Parser $ \str ->
    case runParser (parseTitle hdr) str of
        Right (l, xs) -> case runParser (parseBefore "</header>\n") xs of
            Right (x, _) -> Right (parseHeaderTags (lines x) l, "")
            _ -> Left "Header incomplete"
        Left err -> Left err

parseHeaderTags :: [String] -> Header -> Header
parseHeaderTags [] hdr = hdr
parseHeaderTags (x:xs) hdr = case runParser (parseHeaderTag hdr) x of
    Right (l, "") -> parseHeaderTags xs l
    _ -> hdr

parseHeaderTag :: Header -> Parser Header
parseHeaderTag hdr = Parser $ \str ->
    case runParser parseChevron str of
        Right ("author", _) -> runParser (parseAuthor hdr) str
        Right ("date", _) -> runParser (parseDate hdr) str
        _ -> Left "Invalid tag"

parseTitle :: Header -> Parser Header
parseTitle hdr = Parser $ \str ->
    case runParser (parseAttributeName "header") str of
        Right ("title", xs) -> case runParser parseAttributeValue xs of
            Right (y, ys) -> case runParser (parseString "\">\n") ys of
                Right (_, zs) -> Right (hdr {title = y}, zs)
                _ -> Left "Title field not closed properly"
            _ -> Left "Title field invalid"
        Right (x, _) -> Left ("Field " ++ x ++ " is invalid")
        Left err -> Left err

parseAuthor :: Header -> Parser Header
parseAuthor hdr = parseBetweenTwo "<author>" "</author>" >>= \authorRes ->
    return hdr { author = Just authorRes }

parseDate :: Header -> Parser Header
parseDate hdr = parseBetweenTwo "<date>" "</date>" >>= \dateRes ->
    return hdr { date = Just dateRes }

parseXml :: Parser Document
parseXml = Parser $ \str ->
    case runParser (parseBetweenTwo "<document>\n" "</document>") str of
        Right (xs, _) -> case runParser (parseHeader defaultHeader) xs of
            Right (hdr, ys) -> case runParser (parseContent) str of
                Right (content, _) -> Right (Document hdr content, "")
                Left err -> Left err
                _ -> Left "Invalid XML, no content found"
            _ -> Left "Invalid XML, no header found"
        _ -> Left "Invalid XML, no document tag found"

parseEntries :: [String] -> [Entry]
parseEntries [] = []
parseEntries (x:xs) = case runParser parseEntry x of
    Right (entry, "") -> entry : parseEntries xs
    _ -> parseEntries xs

parseContent :: Parser [Entry]
parseContent = Parser $ \str ->
    case runParser (parseBefore "</header>\n") str of
        Right (_, xs) -> case runParser (parseBetweenTwo "<body>\n" "</body>\n") xs of
            Right (ys, _) -> parseEntries (lines ys)
            Left err -> Left err
        Left err -> Left err
        _ -> Left "No body found"

parseEntry :: Parser Entry
parseEntry =
    Text <$> parseBetweenTwo "<paragraph>" "</paragraph>" *> parseEntry <|>
    Bold <$> (parseBetweenTwo "<bold>" "</bold>" *> parseEntry)
    -- <|>
    -- Italic <$> (parseBetweenTwo "<italic>" "</italic>" *> parseEntry) <|>
    -- Code <$> (parseBetweenTwo "<code>" "</code>" *> parseEntry) <|>
    -- Section <$> (parseAttribute "<section title=\"" "\">" >>= \title -> many parseEntry) <|>
    -- CodeBlock <$> (parseBetweenTwo "<codeblock>" "</codeblock>" >>= \content -> many parseEntry) <|>
    -- List <$> (parseBetweenTwo "<list>" "</list>" >>= \content -> many parseEntry) <|>
    -- Link <$> (parseBetweenTwo "<link url=\"" "\">" >>= \url -> parseEntry) <|>
    -- Image <$> (parseBetweenTwo "<image url=\"" "\">" >>= \url -> (parseBetweenTwo "<paragraph>" "</paragraph>"))
