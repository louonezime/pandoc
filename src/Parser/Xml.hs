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

-- parseTextContent :: Parser String
-- parseTextContent = parseSome (parseSomeChar (['a'..'z'] ++ ['A'..'Z'] ++ " ,.!?"))

-- parseXMLElement :: String -> Parser Entry
-- parseXMLElement "paragraph" = Paragraph <$> (parseTag "paragraph" *> parseTextContent <* parseTag "/paragraph")
-- parseXMLElement "bold" = Bold <$> (parseTag "bold" *> parseTextContent <* parseTag "/bold")
-- parseXMLElement "italic" = Italic <$> (parseTag "italic" *> parseTextContent <* parseTag "/italic")
-- parseXMLElement "code" = Code <$> (parseTag "code" *> parseTextContent <* parseTag "/code")
-- parseXMLElement "codeblock" = CodeBlock <$> (parseTag "codeblock" *> parseTextContent <* parseTag "/codeblock")
-- parseXMLElement "list" = List <$> (parseTag "list" *> parseMany (parseXMLElement "paragraph") <* parseTag "/list")
-- parseXMLElement "link" = do
--   url <- parseTag "link" *> parseAttribute "url" <* parseChar '>'
--   content <- parseTextContent <* parseTag "/link"
--   return (Link url [Paragraph content])
-- parseXMLElement "image" = do
--   url <- parseTag "image" *> parseAttribute "url" <* parseChar '>'
--   altText <- parseTextContent <* parseTag "/image"
--   return (Image url [Paragraph altText])
-- parseXMLElement "section" = do
--   title <- parseTag "section" *> parseAttribute "title" <* parseChar '>'
--   content <- parseMany parseXML <* parseTag "/section"
--   return (Section title content)

-- parseXML :: Parser Document
-- parseXML = do
--    parseChevron "document"
--    header <- parseXMLElement "header"
--    body <- parseXMLElement "body"
--    parseEndChevron "document"
--    return (Document header [body])
