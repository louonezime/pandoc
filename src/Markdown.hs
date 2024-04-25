{-
-- EPITECH PROJECT, 2024
-- Markdown
-- File description:
-- pandoc
-}

module Markdown (parseMarkdown, parseBody) where

import Control.Applicative ((<|>))
import Debug.Trace (traceShowId)
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
parseText = Parser $ \s -> Right (Text s, [])

parseFormat :: String -> (Entry -> Entry) -> Parser Entry
parseFormat sep fmt = Parser $ \s -> case runParser (parseBetween sep) s of
    Right (x, xs) -> case runParser (fmt <$> parseText) x of
        Right (y, _) -> Right (y, xs)
        Left e -> Left e
    Left e -> Left e

parseBold :: Parser Entry
parseBold = parseFormat "**" Bold

parseCode :: Parser Entry
parseCode = parseFormat "`" Code

parseItalic :: Parser Entry
parseItalic = parseFormat "*" Italic

parseFormatElement :: Parser Entry
parseFormatElement = (Text <$> parseSome (parseNonStr "*`")) <|> parseBold <|> parseItalic <|> parseCode <|> parseText

parseParagraphContent :: Parser [Entry]
parseParagraphContent = parseMany parseFormatElement

parseParagraph :: Parser Entry
parseParagraph =
    Paragraph
        <$> Parser
            ( \s -> case runParser parseLine s of
                Right (x, xs) -> case runParser parseParagraphContent x of
                    Right (y, _) -> Right (y, xs)
                    Left _ -> Right ([], xs)
                Left _ -> Left "Not a paragraph"
            )

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
