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
