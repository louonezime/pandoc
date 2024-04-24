{-
-- EPITECH PROJECT, 2024
-- Markdown
-- File description:
-- pandoc
-}

import Data.List
import Data.Char
import Data.Either

data MarkdownElement
    = MarkdownHeader [MarkdownElement]
    | MarkdownTitle String
    | MarkdownAuthor (Maybe String)
    | MarkdownDate (Maybe String)
    | MarkdownText String
    | MarkdownItalic MarkdownContent
    | MarkdownBold MarkdownContent
    | MarkdownCode MarkdownContent
    | MarkdownLink String String
    | MarkdownImage String String
    | MarkdownParagraph [MarkdownElement]
    | MarkdownSection String [MarkdownElement]
    | MarkdownCodeBlock String
    | MarkdownList [MarkdownElement]
    | MarkdownItem MarkdownContent
    deriving (Show)

type MarkdownContent = [MarkdownElement]

-- parseHeader ["title: Syntaxe MARKDOWN", "author: John Doe", "date: 2024-04-23"]

parseHeader :: [String] -> MarkdownContent
parseHeader ("---\n":rest) = [] -- end of header 
parseHeader (line:rest)
    | "title: " `isPrefixOf` line = MarkdownTitle (drop 7 line) : parseHeader rest
    | "author: " `isPrefixOf` line = MarkdownAuthor (Just $ drop 8 line) : parseHeader rest
    | "date: " `isPrefixOf` line = MarkdownDate (Just $ drop 6 line) : parseHeader rest
    | otherwise = parseHeader rest
parseHeader [] = []


isHeader :: String -> Either String MarkdownElement
isHeader str
    | str == "---\n" = Left "End of header"
    | otherwise = Right (MarkdownHeader (parseHeader [str]))

parseMarkdown :: [String] -> MarkdownContent
parseMarkdown = rights . map isHeader

-- let exampleMarkdown = ["---\n", "title: Syntaxe MARKDOWN", "author: John Doe",
-- "date: 2024-04-23", "---\n", "Content goes here", "rest of the file"]
-- parseMarkdown exampleMarkdown

-- test ghci

-- data Parser a = Parser {
--     runParser :: String -> Maybe (a, String)
-- }

-- ---
-- title: Syntaxe MARKDOWN
-- author: Fornes Leo
-- date: 2024-01-01
-- ---