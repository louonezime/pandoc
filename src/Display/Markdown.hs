{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Markdown
-}

module Display.Markdown (
    renderMarkdown,
) where

import Display.Json ()
import Display.Xml ()
import Document (Document (..), Entry (..), Header (..))

renderMarkdown :: Document -> String
renderMarkdown (Document hd bdy) = renderHeader hd ++ renderBody bdy

renderHeader :: Header -> String
renderHeader (Header t a d) =
    "---\n" ++ renderTitle t ++ renderAuthor a ++ renderDate d ++ "---\n"

renderTitle :: String -> String
renderTitle s = "title: " ++ s ++ "\n"

renderDate :: Maybe String -> String
renderDate Nothing = ""
renderDate (Just s) = "date: " ++ s ++ "\n"

renderAuthor :: Maybe String -> String
renderAuthor Nothing = ""
renderAuthor (Just s) = "author: " ++ s ++ "\n"

renderBold :: Int -> Entry -> String
renderBold n ct = '*' : '*' : renderElem n ct ++ "**"

renderItalic :: Int -> Entry -> String
renderItalic n ct = '*' : renderElem n ct ++ "*"

renderCode :: Int -> Entry -> String
renderCode n ct = '`' : renderElem n ct ++ "`"

renderParagraph :: Int -> [Entry] -> String
renderParagraph n els = renderElems n els ++ "\n"

renderSection :: Int -> String -> [Entry] -> String
renderSection n t els =
    take n (cycle "#") ++ t ++ '\n' : renderElems (n + 1) els

renderList :: Int -> [Entry] -> String
renderList _ [] = ""
renderList n (x : xs) = "- " ++ renderElem n x ++ "\n" ++ renderList n xs

renderLink :: Int -> String -> Entry -> String
renderLink n ul el = '[' : renderElem n el ++ "](" ++ ul ++ ")"

renderImage :: Int -> String -> Entry -> String
renderImage n ul el = "![" ++ renderElem n el ++ "](" ++ ul ++ ")"

renderCodeBlock :: [Entry] -> String
renderCodeBlock els =
    "```" ++ renderElems 1 els ++ "```\n"

renderElem :: Int -> Entry -> String
renderElem n (Bold b) = renderBold n b
renderElem n (Italic b) = renderItalic n b
renderElem n (Code b) = renderCode n b
renderElem n (Paragraph p) = renderParagraph n p
renderElem n (Section t ct) = renderSection n t ct
renderElem n (List ct) = renderList n ct
renderElem n (Link ul at) = renderLink n ul at
renderElem n (Image ul at) = renderImage n ul at
renderElem _ (Text s) = s
renderElem _ (CodeBlock c) = renderCodeBlock c

renderElems :: Int -> [Entry] -> String
renderElems _ [] = "\n"
renderElems n (x : xs) = renderElem n x ++ renderElems n xs

renderBody :: [Entry] -> String
renderBody = renderElems 1
