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
import Document (Document (..), Entry (Text, CodeBlock), Header (..))
import Parsing

-- RAPPEL
-- newtype Parser a = Parser {
--     runParser :: String -> Either String (a, String)
-- }

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

parseMarkdown :: Parser Document
parseMarkdown =
    Document <$> parseHeader (Header "" Nothing Nothing) <*> parseBody

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
        Right (s, "") -> Left ("No " ++ s ++ "specified")
        Right ("title: ", z) -> Right (hd {title = z}, "")
        Right ("title:", z) -> Right (hd {title = z}, "")
        Right ("date: ", z) -> Right (hd {date = Just z}, "")
        Right ("date:", z) -> Right (hd {date = Just z}, "")
        Right ("author: ", z) -> Right (hd {author = Just z}, "")
        Right ("author:", z) -> Right (hd {author = Just z}, "")
        _ -> Left "No field found"

parseField :: Parser String
parseField = parseTitle <|> parseDate <|> parseAuthor

parseTitle :: Parser String
parseTitle = parseOr (parseAfter "title: ") (parseAfter "title:")

parseDate :: Parser String
parseDate = parseOr (parseAfter "date: ") (parseAfter "date:")

parseAuthor :: Parser String
parseAuthor = parseOr (parseAfter "author: ") (parseAfter "author:")

parseBody :: Parser [Entry]
parseBody = parseSome parseEntry

parseEntry :: Parser Entry
parseEntry = parseText <|> (CodeBlock <$> parseCodeBlock)

parseText :: Parser Entry
parseText = Parser $ \s -> Right (Text s, "")

parseBold :: Parser Entry
parseBold = Parser $ \s -> case runParser (parseBetween "**") s of
    Right (x, xs) -> Right (Text x, xs)
    Left e -> Left e

parseCodeBlockDelim :: Parser String
parseCodeBlockDelim = parseBetween "```\n"

parseCodeBlock :: Parser [Entry]
parseCodeBlock = do
    codeContent <- parseBetween "```\n"
    let linesOfCode = lines codeContent
        codeEntries = map Text linesOfCode
    return codeEntries


-- to test this run
-- runParser (parser) "string"
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
