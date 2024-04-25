{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Xml
-}

module Parser.Xml where

import Parsing
import Document

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
        Right (l, xs) -> case runParser (parseBefore "</header>") xs of
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
        _ -> case runParser (parseEndChevron) str of
            Right ("header", _) -> Right (hdr, "")
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
parseAuthor hdr = Parser $ \str ->
    case runParser (parseBetweenTwo "<author>" "</author>") str of
        Right (xs, "") -> Right (hdr {author = Just xs}, "")
        _ -> Left "Author field invalid"

parseDate :: Header -> Parser Header
parseDate hdr = Parser $ \str ->
    case runParser (parseBetweenTwo "<date>" "</date>") str of
        Right (xs, "") -> Right (hdr {date = Just xs}, "")
        _ -> Left "Date field invalid"
