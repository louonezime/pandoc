{-
-- EPITECH PROJECT, 2024
-- Markdown
-- File description:
-- pandoc
-}

module Parser.Markdown (parseMarkdown, parseBody, parseLink) where

import Control.Applicative ((<|>))
import Debug.Trace (traceShowId)
import Document (Document (..), Entry (..), Header (..))
import Parsing (
    Parser (..),
    parseAfter,
    parseAndWith,
    parseBetween,
    parseBetweenTwo,
    parseLine,
    parseNonStr,
    parseOr,
    parseSome,
    parseStringAndThen,
    parseTillEmpty,
 )

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
parseEntry =
    parseParagraph
        <|> (CodeBlock <$> parseCodeBlock)

parseText :: Parser Entry
parseText = Parser $ \s -> Right (Text s, [])

parseFormat :: String -> (Entry -> Entry) -> Parser Entry
parseFormat sep fmt = parseStringAndThen (parseBetween sep) (fmt <$> parseText)

parseBold :: Parser Entry
parseBold = parseFormat "**" Bold

parseCode :: Parser Entry
parseCode = parseFormat "`" Code

parseItalic :: Parser Entry
parseItalic = parseFormat "*" Italic

parseBoldItalic :: Parser Entry
parseBoldItalic = parseFormat "***" (Italic <$> Bold)

parseFormatElement :: Parser Entry
parseFormatElement =
    (Text <$> parseSome (parseNonStr "*`[!"))
        <|> parseBoldItalic
        <|> parseBold
        <|> parseItalic
        <|> parseCode
        <|> parseLink
        <|> parseImage
        <|> parseText

parseLinkFormat :: Parser Entry
parseLinkFormat =
    (Text <$> parseSome (parseNonStr "*`!"))
        <|> parseBoldItalic
        <|> parseBold
        <|> parseItalic
        <|> parseCode
        <|> parseImage
        <|> parseText

parseImageFormat :: Parser Entry
parseImageFormat =
    (Text <$> parseSome (parseNonStr "*`!"))
        <|> parseBoldItalic
        <|> parseBold
        <|> parseItalic
        <|> parseCode
        <|> parseLink
        <|> parseText

parseParagraphContent :: Parser [Entry]
parseParagraphContent = parseTillEmpty parseFormatElement

parseParagraph :: Parser Entry
parseParagraph =
    Paragraph
        <$> parseStringAndThen parseLine parseParagraphContent

parseCodeBlockDelim :: Parser String
parseCodeBlockDelim = parseBetween "```\n"

parseCodeBlock :: Parser [Entry]
parseCodeBlock = Parser $ \str ->
    case runParser parseCodeBlockDelim str of
        Right (codeBlock, rest) -> Right (map Text (lines codeBlock), rest)
        Left err -> Left err

parseLinkAlt :: Parser Entry
parseLinkAlt = parseStringAndThen (parseBetweenTwo "[" "]") parseLinkFormat

parseImageAlt :: Parser Entry
parseImageAlt = parseStringAndThen (parseBetweenTwo "![" "]") parseImageFormat

parseUrl :: Parser String
parseUrl = parseBetweenTwo "(" ")"

parseLink :: Parser Entry
parseLink =
    uncurry Link
        <$> Parser
            ( \s -> case runParser parseLinkAlt s of
                Right (x, xs) -> case runParser parseUrl xs of
                    Right (y, ys) -> Right ((y, x), ys)
                    Left e -> Left e
                Left e -> Left e
            )

parseImage :: Parser Entry
parseImage = do
    uncurry Image
        <$> Parser
            ( \s -> case runParser parseImageAlt s of
                Right (x, xs) -> case runParser parseUrl xs of
                    Right (y, ys) -> Right ((y, x), ys)
                    Left e -> Left e
                Left e -> Left e
            )
