{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Xml
-}

module Xml where

import Control.Applicative (Alternative (..))
import Control.Monad ((>=>))

import Parsing
import Document

parseAttributeValue :: Parser String
parseAttributeValue = parseQuotes

parseAttributeName :: Parser String
parseAttributeName = parseSome (parseNonStr " =") <* parseChar '='

parseChevron :: Parser String
parseChevron = parseChar '<' *> parseSome (parseNonStr "</>") <* parseChar '>'

parseEndChevron :: Parser String
parseEndChevron = parseChar '<' *> parseChar '/' *> parseSome (parseNonStr "<>") <* parseChar '>'

-- parseTag :: String -> Parser String
-- parseTag name = parseChar '<' *> parseChar '/' *> parseChar name *> parseChar '>'
-- parseTag name = parseChar '<' *> parseChar name *> parseMany (parseAttribute name) <* parseChar '>'

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

parseXML :: Parser Document
parseXML = do
  parseChevron "document"
  -- header <- parseXMLElement "header"
  -- body <- parseXMLElement "body"
  parseEndChevron "document"
  return (Document header [body])
