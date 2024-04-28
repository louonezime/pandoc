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
            Right (hdr, ys) -> case runParser (parseBetweenTwo "<body>\n" "</body>\n") ys of
                Right (_, zs) -> case runParser parseContent zs of
                    Right (ctx, _) -> Right (Document hdr ctx, [])
                    Left err -> Left err
                Left err -> Left err
            _ -> Left "Invalid XML, no header found"
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
        <$> Parser (\str -> case runParser parseSection' str of
                    Right (x, xs) -> case runParser (parseSection'' x) xs of
                        Right (y, ys) -> case runParser parseContent ys of
                            Right (z, zs) -> Right ((y, z), zs)
                            Left err -> Left err
                        Left err -> Left err
                    Left err -> Left err)

-- inputString :: String
-- inputString = "<document>\n\t<header title=\"Syntaxe XML\">\n\t\t<author>Fornes Leo</author>\n\t\t<date>2024-01-01</date>\n\t</header>\n\t<body>\n\t\t<paragraph>This document is a simple example of the XML syntax.</paragraph>\n\t\t<paragraph>Every syntax element is displayed in this document.</paragraph>\n\t\t<section title=\"header 1\">\n\t\t\t<paragraph>This is a basic paragraph with text.</paragraph>\n\t\t\t<paragraph>This is a paragraph with <bold>bold</bold>, <italic>italic</italic> and <code>code</code> text.</paragraph>\n\t\t\t<section title=\"header 2\">\n\t\t\t\t<codeblock>\n\t\t\t\t\t<paragraph>This is a code block.</paragraph>\n\t\t\t\t</codeblock>\n\t\t\t\t<list>\n\t\t\t\t\t<paragraph>list item 1</paragraph>\n\t\t\t\t\t<paragraph>list item 2</paragraph>\n\t\t\t\t\t<paragraph>list item 3</paragraph>\n\t\t\t\t</list>\n\t\t\t\t<paragraph>This is a paragraph with a <link url=\"https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley\">link</link>.</paragraph>\n\t\t\t\t<paragraph>This is a paragraph with an image<image url=\"https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png\">Text to replace image</image>.</paragraph>\n\t\t\t\t<section title=\"\">\n\t\t\t\t\t<section title=\"header 4\">\n\t\t\t\t\t\t<paragraph>Every syntax element can be use separately or combined.</paragraph>\n\t\t\t\t\t\t<paragraph>Think about the different possible combinations.</paragraph>\n\t\t\t\t\t\t<paragraph>All combined syntax elements aren't displayed in this document.</paragraph>\n\t\t\t\t\t</section>\n\t\t\t\t</section>\n\t\t\t</section>\n\t\t</section>\n\t</body>\n\t</document>"
inputString :: String
inputString = "<document>\n<header title=\"Syntaxe XML\">\n<author>Fornes Leo</author>\n<date>2024-01-01</date>\n</header>\n<body>\n<paragraph>This document is a simple example of the XML syntax.</paragraph>\n<paragraph>Every syntax element is displayed in this document.</paragraph>\n<section title=\"header 1\">\n<paragraph>This is a basic paragraph with text.</paragraph>\n<paragraph>This is a paragraph with <bold>bold</bold>, <italic>italic</italic> and <code>code</code> text.</paragraph>\n<section title=\"header 2\">\n<codeblock>\n<paragraph>This is a code block.</paragraph>\n</codeblock>\n<list>\n<paragraph>list item 1</paragraph>\n<paragraph>list item 2</paragraph>\n<paragraph>list item 3</paragraph>\n</list>\n<paragraph>This is a paragraph with a <link url=\"https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley\">link</link>.</paragraph>\n<paragraph>This is a paragraph with an image<image url=\"https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png\">Text to replace image</image>.</paragraph>\n<section title=\"\">\n<section title=\"header 4\">\n<paragraph>Every syntax element can be use separately or combined.</paragraph>\n<paragraph>Think about the different possible combinations.</paragraph>\n<paragraph>All combined syntax elements aren't displayed in this document.</paragraph>\n</section>\n</section>\n</section>\n</section>\n</body>\n</document>"