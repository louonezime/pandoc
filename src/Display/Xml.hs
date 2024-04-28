{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Xml
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Display.Xml (
    renderXml,
    renderHeader,
    renderTitle,
    renderAuthor,
    renderDate,
    renderCode,
    renderBold,
    renderItalic,
    renderLink,
    renderImage,
    renderList,
    renderParagraph,
    renderSection,
    renderCodeBlock,
) where

import Document (Document (..), Entry (..), Header (..))

tab :: Int -> String
tab n = replicate n '\t'

renderXml :: Document -> String
renderXml (Document hd bdy) =
    "<document>" ++ renderHeader hd ++ renderBody bdy ++ "</document>\n"

renderTitle :: String -> String
renderTitle s = '"' : s ++ "\""

renderAuthor :: Maybe String -> String
renderAuthor Nothing = ""
renderAuthor (Just s) = "\t\t<author>" ++ s ++ "</author>\n"

renderDate :: Maybe String -> String
renderDate Nothing = ""
renderDate (Just s) = "\t\t<date>" ++ s ++ "</date>\n"

renderHeader :: Header -> String
renderHeader (Header t a d) =
    "\n\t<header title="
        ++ renderTitle t
        ++ ">\n"
        ++ renderAuthor a
        ++ renderDate d
        ++ "\t</header>\n"

renderBody :: [Entry] -> String
renderBody arr =
    tab 1
        ++ "<body>\n"
        ++ renderElems arr 1
        ++ tab 1
        ++ "</body>\n"

renderElems :: [Entry] -> Int -> String
renderElems elems n = concatMap (renderElem n) elems

renderBold :: Int -> Entry -> String
renderBold n (Bold s) =
    tab n
        ++ "<bold>\n"
        ++ renderElem (n + 1) s
        ++ tab n
        ++ "</bold>\n"

renderItalic :: Int -> Entry -> String
renderItalic n (Italic s) =
    tab n
        ++ "<italic>\n"
        ++ renderElem (n + 1) s
        ++ tab n
        ++ "</italic>\n"

renderCode :: Int -> Entry -> String
renderCode n (Code s) =
    tab n
        ++ "<code>\n"
        ++ renderElem (n + 1) s
        ++ tab n
        ++ "</code>\n"

renderList :: Int -> Entry -> String
renderList n (List arr) =
    tab n
        ++ "<list>\n"
        ++ renderElems arr (n + 1)
        ++ tab n
        ++ "</list>\n"

renderParagraph :: Int -> Entry -> String
renderParagraph n (Paragraph arr) =
    tab n
        ++ "<paragraph>\n"
        ++ renderElems arr (n + 1)
        ++ tab n
        ++ "</paragraph>\n"

renderImage :: Int -> Entry -> String
renderImage n (Image l t) =
    tab n
        ++ "<image url=\""
        ++ l
        ++ "\">"
        ++ renderElem (n + 1) t
        ++ tab n
        ++ "</image>\n"

renderLink :: Int -> Entry -> String
renderLink n (Link l t) =
    tab n
        ++ "<link url=\""
        ++ l
        ++ "\">"
        ++ renderElem (n + 1) t
        ++ tab n
        ++ "</link>\n"

renderSection :: Int -> Entry -> String
renderSection n (Section t arr) =
    tab n
        ++ "<section title=\""
        ++ t
        ++ "\">"
        ++ renderElems arr (n + 1)
        ++ tab n
        ++ "</section>\n"

renderCodeBlock :: Int -> Entry -> String
renderCodeBlock n (CodeBlock arr) =
    tab n
        ++ "<codeblock>"
        ++ renderElems arr n
        ++ tab (n + 1)
        ++ "</codeblock>\n"

renderElem :: Int -> Entry -> String
renderElem _ (Text t) = t
renderElem n x@(Bold _) = renderBold n x
renderElem n x@(Italic _) = renderItalic n x
renderElem n x@(Code _) = renderCode n x
renderElem n x@(List _) = renderList n x
renderElem n x@(Paragraph _) = renderParagraph n x
renderElem n x@(Link _ _) = renderLink n x
renderElem n x@(Image _ _) = renderImage n x
renderElem n x@(Section _ _) = renderSection n x
renderElem n x@(CodeBlock _) = renderCodeBlock n x
