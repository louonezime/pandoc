{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Xml
-}

module ParseXml where

import Control.Applicative (Alternative (..))
import Control.Monad ((>=>))

import Parsing
import Document

parseAttributeValue :: Parser String
parseAttributeValue = parseChar '=' *> parseChar '\"' *> parseSome (parseNonStr "\"")

parseAttributeName :: Parser String
parseAttributeName = parseChar '<' *> parseSome (parseNonStr " ") *> parseChar ' ' *> parseSome (parseNonStr " =")

parseChevron :: Parser String
parseChevron = parseChar '<' *> parseSome (parseNonStr "</>") <* parseChar '>'

parseEndChevron :: Parser String
parseEndChevron = parseChar '<' *> parseChar '/' *> parseSome (parseNonStr "<>") <* parseChar '>'

parseHeader :: Header -> Parser Header
parseHeader hd = Parser $ \str ->
    case runParser parseChevron str of
        Right (x, xs) -> Right (parseHeaderTags (lines x) hd, xs)
        _ -> Left "Header Invalid"

parseHeaderTags :: [String] -> Header -> Header
parseHeaderTags [] header = header
parseHeaderTags (x : xs) header = case runParser (parseHeaderTag header) x of
    Right (r, "") -> parseHeaderTags xs r
    _ -> header

parseTitle :: Header -> Parser Header
parseTitle header = Parser $ \str ->
    case runParser parseAttributeName str of
        Right ("title", xs) -> case runParser parseAttributeValue xs of
            Right (ys, "\">") -> Right (header {title = ys}, "")
            _ -> Left "Title field invalid"
        Right (x, xs) -> Left ("Field " ++ x ++ " is invalid")
        Left err -> Left err

parseHeaderTag :: Header -> Parser Header
parseHeaderTag header = Parser $ \str ->
    case runParser parseChevron str of
        Right ("header", xs) -> case runParser parseChevron xs of
            Right ("date", ys) -> Right (header {date = Just ys}, "")
            Right ("author", ys) -> Right (header {author = Just ys}, "")
            _ -> Left "No header field found"
        Right (xs, "") -> runParser (parseTitle header) str
        _ -> Left "Invalid header"

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
