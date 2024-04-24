{-
-- EPITECH PROJECT, 2024
-- Markdown
-- File description:
-- pandoc
-}

module Markdown (parseMarkdown, parseBody) where

import Control.Applicative ((<|>))
import Document (Document (..), Entry (..), Header (..))
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

parseMarkdown :: Parser Document
parseMarkdown =
    parseAndWith Document (parseHeader (Header "" Nothing Nothing)) (pure [])

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

parseBody :: Parser Entry
parseBody = parseEntry

parseEntry :: Parser Entry
parseEntry = parseParagraph

parseText :: Parser Entry
parseText = Parser $ \s -> Right (Text s, "")

parseInnerParagraph :: Parser Entry
parseInnerParagraph = parseBold <|> parseCode <|> parseText

parseBold :: Parser Entry
parseBold = Parser $ \s -> case runParser (parseBetween "**") s of
    Right (x, _) -> runParser (Bold <$> parseInnerParagraph) x
    Left e -> Left e

parseCode :: Parser Entry
parseCode = Parser $ \s -> case runParser (parseBetween "`") s of
    Right (x, _) -> runParser (Code <$> parseInnerParagraph) x
    Left e -> Left e

-- parseItalic :: Parser Entry
-- parseItalic = Parser $ \s ->
--     case runParser (parseChar '*' *> parseSome (parseNonStr "*") <* parseChar '*') s of
--         Right (x, _) -> runParser (Italic <$> parseInnerParagraph) x
--         Left e -> Left e

parseParagraph :: Parser Entry
parseParagraph = Parser $ \s ->
    case runParser parseLine s of
        Right (x, _) -> runParser parseInnerParagraph x
        Left _ -> Left "not a paragraph"

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
