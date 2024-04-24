{-
-- EPITECH PROJECT, 2024
-- Markdown
-- File description:
-- pandoc
-}

module Markdown (parseMarkdown) where

import Data.Char
import Data.Either
import Data.List

import Control.Applicative ((<|>))
import Document (Document (..), Header (..))
import Parsing

data MarkdownElement
    = MarkdownHeader [MarkdownElement]
    | MarkdownTitle String
    | MarkdownAuthor (Maybe String)
    | MarkdownDate (Maybe String)
    | MarkdownCodeBlock [String]
    | MarkdownText [String]
    | MarkdownItalic MarkdownContent
    | MarkdownBold MarkdownContent
    | MarkdownLink String String
    | MarkdownImage String String
    | MarkdownParagraph [MarkdownElement]
    | MarkdownSection String [MarkdownElement]
    | MarkdownList [MarkdownElement]
    | MarkdownItem MarkdownContent
    deriving (Show)

type MarkdownContent = [MarkdownElement]

parseCodeBlock :: [String] -> MarkdownContent
parseCodeBlock ("```\n" : rest) = [] -- end of code block
parseCodeBlock (line : rest) = MarkdownCodeBlock [line] : parseCodeBlock rest

isCodeBlock :: String -> Either String MarkdownElement
isCodeBlock str
    | str == "```\n" = Left "End of code block"
    | otherwise = Right (MarkdownCodeBlock [str])

-- parseHeader :: [String] -> MarkdownContent
-- parseHeader ("---\n" : rest) = [] -- end of header
-- parseHeader (line : rest)
--     | "title: " `isPrefixOf` line = MarkdownTitle (drop 7 line) : parseHeader rest
--     | "author: " `isPrefixOf` line = MarkdownAuthor (Just $ drop 8 line) : parseHeader rest
--     | "date: " `isPrefixOf` line = MarkdownDate (Just $ drop 6 line) : parseHeader rest
--     | otherwise = parseHeader rest
-- parseHeader [] = []

-- parseHeader ["title: Syntaxe MARKDOWN", "author: John Doe", "date: 2024-04-23"]

-- isHeader :: String -> Either String MarkdownElement
-- isHeader str
--     | str == "---\n" = Left "End of header"
--     | otherwise = Right (MarkdownHeader (parseHeader [str]))

-- parseMarkdown :: [String] -> MarkdownContent
-- parseMarkdown = rights . concatMap (\str -> [isHeader str, isCodeBlock str])

createDocFromHeader :: Header -> Document
createDocFromHeader hd = Document hd []

parseMarkdown :: Parser Document
parseMarkdown = createDocFromHeader <$> parseHeader (Header "" Nothing Nothing)

parseHeaderDash :: Parser String
parseHeaderDash = parseBetween "---\n"

parseHeader :: Header -> Parser Header
parseHeader hd = Parser $ \str ->
    case runParser parseHeaderDash str of
        Right (x, xs) -> Right (parseHeaderFields (lines x) hd, xs)
        _ -> Left "Header Not found"

parseHeaderFields :: [String] -> Header -> Header
parseHeaderFields [] hd = hd
parseHeaderFields (x : xs) hd = case runParser (parseHeaderField hd) x of
    Right (r, "") -> parseHeaderFields xs r
    _ -> hd

parseHeaderField :: Header -> Parser Header
parseHeaderField hd = Parser $ \str ->
    case runParser parseField str of
        Right ("title: ", "") -> Left "No title specified"
        Right ("title: ", z) -> Right (hd {title = z}, "")
        Right ("date: ", z) -> Right (hd {date = Just z}, "")
        Right ("author: ", z) -> Right (hd {author = Just z}, "")
        _ -> Left "No field found"

parseField :: Parser String
parseField = parseTitle <|> parseDate <|> parseAuthor

parseTitle :: Parser String
parseTitle = parseAfter "title: "

parseDate :: Parser String
parseDate = parseAfter "date: "

parseAuthor :: Parser String
parseAuthor = parseAfter "author: "

-- parseMarkdown exampleMarkdown
-- let exampleMarkdown = ["---\n", "title: Example", "author: John Doe", "date: 2024-04-23",
-- "---\n", "```\n", "Code block line 1", "Code block line 2", "```", "Rest of the file"]
-- parseCodeBlock test
-- let exampleMarkdown = ["---\n", "title: Syntaxe MARKDOWN", "author: John Doe",
-- "date: 2024-04-23", "---\n", "Content goes here", "rest of the file"]
-- parseHeader test

-- data Parser a = Parser {
--     runParser :: String -> Maybe (a, String)
-- }

-- ---
-- title: Syntaxe MARKDOWN
-- author: Fornes Leo
-- date: 2024-01-01
-- ---
