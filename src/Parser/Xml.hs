{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Xml
-}

module Parser.Xml (parseXml, parseEntry) where

import Control.Applicative (Alternative (..))
import Document (Document (..), Entry (..), Header (..), defaultHeader)
import Parsing (
    Parser (..),
    parseBefore,
    parseBetweenTwo,
    parseChar,
    parseLine,
    parseNonStr,
    parseSome,
    parseString,
    parseStringAndThen,
    parseTillEmpty,
 )

parseAttributeValue :: Parser String
parseAttributeValue = parseString "=\"" *> parseSome (parseNonStr "\"")

parseAttributeName :: String -> Parser String
parseAttributeName str =
    parseString ("<" ++ str ++ " ") *> parseSome (parseNonStr " =")

parseChevron :: Parser String
parseChevron = parseChar '<' *> parseSome (parseNonStr "</>") <* parseChar '>'

parseEndChevron :: Parser String
parseEndChevron =
    parseString "</" *> parseSome (parseNonStr "<>") <* parseChar '>'

parseHeader :: Header -> Parser Header
parseHeader hdr = Parser $ \str ->
    case runParser (parseTitle hdr) str of
        Right (l, xs) -> case runParser (parseBefore "</header>\n") xs of
            Right (x, ys) -> Right (parseHeaderTags (lines x) l, ys)
            _ -> Left "Header incomplete"
        Left err -> Left err

parseHeaderTags :: [String] -> Header -> Header
parseHeaderTags [] hdr = hdr
parseHeaderTags (x : xs) hdr = case runParser (parseHeaderTag hdr) x of
    Right (l, "") -> parseHeaderTags xs l
    _ -> hdr

parseHeaderTag :: Header -> Parser Header
parseHeaderTag hdr = Parser $ \str ->
    case runParser parseChevron str of
        Right ("author", _) -> runParser (parseAuthor hdr) str
        Right ("date", _) -> runParser (parseDate hdr) str
        _ -> Left "Invalid tag"

parseEndTitle :: Parser String
parseEndTitle = parseString "\">\n" <|> parseString "\"></header>\n"

parseTitle :: Header -> Parser Header
parseTitle hdr = Parser $ \str ->
    case runParser (parseAttributeName "header") str of
        Right ("title", xs) -> case runParser parseAttributeValue xs of
            Right (y, ys) -> case runParser (parseEndTitle) ys of
                Right (_, zs) -> Right (hdr {title = y}, zs)
                _ -> Left "Title field not closed properly"
        Right (x, _) -> Left ("Field " ++ x ++ " is invalid")
        Left err -> Left err

parseAuthor :: Header -> Parser Header
parseAuthor hdr =
    parseBetweenTwo "<author>" "</author>" >>= \authorRes ->
        return hdr {author = Just authorRes}

parseDate :: Header -> Parser Header
parseDate hdr =
    parseBetweenTwo "<date>" "</date>" >>= \dateRes ->
        return hdr {date = Just dateRes}

parseXml :: Parser Document
parseXml = Parser $ \str ->
    case runParser (parseBetweenTwo "<document>\n" "</document>") str of
        Right (xs, _) -> case runParser (parseHeader defaultHeader) xs of
            Right (hdr, ys) -> case runParser parseContent ys of
                Right (ctx, _) -> Right (Document hdr ctx, [])
                Left err -> Left err
            Left err -> Left err
        _ -> Left "Invalid XML, no document tag found"

parseContent :: Parser [Entry]
parseContent = parseTillEmpty parseEntry

parseEntry :: Parser Entry
parseEntry = parseParagraph <|> parseCodeBlock <|> parseList <|> parseSection

parseParagraph :: Parser Entry
parseParagraph =
    Paragraph
        <$> parseStringAndThen
            ( parseBetweenTwo "<paragraph>\n" "</paragraph>"
                <|> parseBetweenTwo "<paragraph>" "</paragraph>"
            )
            parseParagraphContent

parseText :: Parser Entry
parseText = Text <$> Parser (\s -> Right (s, []))

parseParagraphContent :: Parser [Entry]
parseParagraphContent = parseTillEmpty parseFormatElement

parseFormatElement :: Parser Entry
parseFormatElement =
    (Text <$> parseSome (parseNonStr "<>"))
        <|> parseBold
        <|> parseItalic
        <|> parseCode
        <|> parseLink
        <|> parseImage
        <|> parseText

parseFormat' :: Parser Entry
parseFormat' =
    (Text <$> parseSome (parseNonStr "<>"))
        <|> parseBold
        <|> parseItalic
        <|> parseCode
        <|> parseText

parseFormat :: String -> (Entry -> Entry) -> Parser Entry
parseFormat sep fmt =
    parseStringAndThen
        (parseBetweenTwo ('<' : sep) ("</" ++ sep))
        (fmt <$> parseFormat')

parseBold :: Parser Entry
parseBold = parseFormat "bold>" Bold

parseItalic :: Parser Entry
parseItalic = parseFormat "italic>" Italic

parseCode :: Parser Entry
parseCode = parseFormat "code>" Code

parseImageLink' :: String -> Parser String
parseImageLink' s = Parser $ \str ->
    case runParser (parseBefore ("</" ++ s ++ ">")) str of
        Right (x, _) -> runParser (parseAttributeName s) x
        _ -> Left "Invalid XML, no end link found"

parseImageLink'' :: String -> Parser String
parseImageLink'' "url" = parseAttributeValue
parseImageLink'' a = Parser (\_ -> Left ("Field " ++ a ++ " is invalid"))

parseImageLink :: String -> Parser Entry -> Parser (String, Entry)
parseImageLink s p = Parser $ \str ->
    case runParser (parseImageLink' s) str of
        Right (x, xs) -> case runParser (parseImageLink'' x) xs of
            Right (y, ys) -> case runParser (parseString "\">") ys of
                Right (_, zs) -> case runParser p zs of
                    Right (z, zs') -> Right ((y, z), zs')
                    Left err -> Left err
                _ -> Left "URL field not closed properly"
            _ -> Left "URL field invalid"
        _ -> Left "Invalid XML, no end link found"

parseLink' :: Parser Entry
parseLink' =
    (Text <$> parseSome (parseNonStr "<>"))
        <|> parseBold
        <|> parseItalic
        <|> parseCode
        <|> parseImage
        <|> parseText

parseLink :: Parser Entry
parseLink =
    uncurry Link
        <$> parseImageLink "link" parseLink'

parseImage' :: Parser Entry
parseImage' =
    (Text <$> parseSome (parseNonStr "<>"))
        <|> parseBold
        <|> parseItalic
        <|> parseCode
        <|> parseLink
        <|> parseText

parseImage :: Parser Entry
parseImage =
    uncurry Image
        <$> parseImageLink "image" parseImage'

parseCodeBlock :: Parser Entry
parseCodeBlock =
    CodeBlock
        <$> parseStringAndThen
            ( parseBetweenTwo
                "<codeblock>\n"
                "</codeblock>"
            )
            parseCodeBlockContent

parseCodeBlockContent :: Parser [Entry]
parseCodeBlockContent = parseTillEmpty (Text <$> parseLine)

parseList :: Parser Entry
parseList =
    List
        <$> parseStringAndThen
            ( parseBetweenTwo "<list>\n" "</list>"
                <|> parseBetweenTwo "<list>" "</list>"
            )
            parseContent

parseSection' :: Parser String
parseSection' = Parser $ \str ->
    case runParser (parseBefore "</section>") str of
        Right (x, _) -> runParser (parseAttributeName "title") x
        _ -> Left "Invalid XML, no end link found"

parseSection'' :: String -> Parser String
parseSection'' "title" = parseAttributeValue
parseSection'' s = Parser $ \_ -> Left ("Section field invalid " ++ s)

parseSection :: Parser Entry
parseSection =
    uncurry Section
        <$> Parser
            ( \str -> case runParser parseSection' str of
                Right (x, xs) -> case runParser (parseSection'' x) xs of
                    Right (y, ys) -> case runParser parseContent ys of
                        Right (z, zs) -> Right ((y, z), zs)
                        Left err -> Left err
                    Left err -> Left err
                Left err -> Left err
            )
