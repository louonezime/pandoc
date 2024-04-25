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
parseHeaderTags (x : xs) header = case   (parseHeaderTag header) x of
    Right (r, "") -> parseHeaderTags xs r
    _ -> header

parseTitle :: Header -> Parser Header
parseTitle header = Parser $ \str ->
    case runParser parseAttributeName str of
        Right ("title", xs) -> case runParser parseAttributeValue xs of
            Right (ys, "\">") -> Right (header {title = ys}, "") >> case runParser parseChevron xs of
                Right ("date", ys) -> Right (header {date = Just ys}, "")
                Right ("author", ys) -> Right (header {author = Just ys}, "")
                _ -> Left "Invalid header"
            _ -> Left "Title field invalid"
        Right (x, xs) -> Left ("Field " ++ x ++ " is invalid")
        Left err -> Left err

parseHeaderTag :: Header -> Parser Header
parseHeaderTag header = Parser $ \str ->
    case runParser parseChevron str of
        Right ("header", xs) -> Left "Header has no title"
        Right (xs, "") -> runParser (parseTitle header) str
        _ -> Left "Invalid header"


-- parseAttributeValue :: Parser String
-- parseAttributeValue = parseChar '\"' *> parseSome (parseNonStr "\"")

-- parseAttributeName :: Parser String
-- parseAttributeName = parseSome (parseNonStr " ") <* parseChar '='

-- parseOpenTag :: Parser String
-- parseOpenTag = parseChar '<' *> parseSome (parseNonStr ">") <* parseChar '>'

-- parseCloseTag :: Parser String
-- parseCloseTag = parseChar '<' *> parseChar '/' *> parseSome (parseNonStr ">") <* parseChar '>'

-- parseHeader :: Header -> Parser Header
-- parseHeader hd = Parser $ \str ->
--     case runParser (parseOpenTag *> parseSome (parseNonStr " ") *> parseOpenTag) str of
--         Right (tagName, rest) | tagName == "header" ->
--             case runParser (parseHeaderContent hd) rest of
--                 Right (newHeader, remaining) ->
--                     Right (newHeader, remaining)
--                 Left err -> Left err
--         Right (x, xs) -> Left ("Invalid header" ++ x ++ " " ++ xs)
--         Left err -> Left err

-- parseHeaderContent :: Header -> Parser Header
-- parseHeaderContent header = parseTitle header <|> parseAuthor header <|> parseDate header

-- parseTitle :: Header -> Parser Header
-- parseTitle header = Parser $ \str ->
--     case runParser (parseOpenTag *> parseAttributeName) str of
--         Right ("title", rest) ->
--             case runParser (parseAttributeValue <* parseChar '>') rest of
--                 Right (titleValue, remaining) ->
--                     Right (header { title = titleValue }, remaining)
--                 Left err -> Left err
--         _ -> Left "Title field invalid"

-- parseAuthor :: Header -> Parser Header
-- parseAuthor header = Parser $ \str ->
--     case runParser (parseOpenTag *> parseAttributeName) str of
--         Right ("author", rest) ->
--             case runParser (parseAttributeValue <* parseChar '>') rest of
--                 Right (authorValue, remaining) ->
--                     Right (header { author = Just authorValue }, remaining)
--                 Left err -> Left err
--         _ -> Left "Author field invalid"

-- parseDate :: Header -> Parser Header
-- parseDate header = Parser $ \str ->
--     case runParser (parseOpenTag *> parseAttributeName) str of
--         Right ("date", rest) ->
--             case runParser (parseAttributeValue <* parseChar '>') rest of
--                 Right (dateValue, remaining) ->
--                     Right (header { date = Just dateValue }, remaining)
--                 Left err -> Left err
--         _ -> Left "Date field invalid"