{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- JsonDisplayer
-}

module Json (
    renderJson,
    renderAuthor,
    renderTitle,
    renderDate,
    renderHeader,
    renderBody,
    renderEntry,
) where

import Document (Document (..), Entry (..), Header (..))

renderJson :: Document -> String
renderJson (Document hdr bd) = '{' : renderHeader hdr ++ renderBody bd ++ "}"

renderHeader :: Header -> String
renderHeader (Header t a d) =
    "\"header\":{" ++ renderTitle t ++ renderAuthor a ++ renderDate d ++ "}"

renderAuthor :: Maybe String -> String
renderAuthor (Just auth) = ",\"author\":\"" ++ auth ++ "\""
renderAuthor Nothing = ""

renderTitle :: String -> String
renderTitle t = "\"title\":\"" ++ t ++ "\""

renderDate :: Maybe String -> String
renderDate (Just d) = ",\"date\":\"" ++ d ++ "\""
renderDate Nothing = ""

app :: String -> String -> String
app s res = res ++ ',' : s

renderBody :: [Entry] -> String
renderBody e = ",\"body\":" ++ renderList e

renderList :: [Entry] -> String
renderList [] = "[]"
renderList (x : xs) = foldr (app . renderEntry) (renderEntry x) xs

renderEntry :: Entry -> String
renderEntry (Text s) = '"' : s ++ "\""
renderEntry (Paragraph p) = '[' : renderList p ++ "]"
renderEntry (Section t c) = renderSection t c
renderEntry (List e) = renderListEntry e
renderEntry (Link ul at) = renderLink ul at
renderEntry (CodeBlock c) = renderCodeBlock c
renderEntry (Bold b) = renderBold b
renderEntry (Italic i) = renderItalic i
renderEntry (Code c) = renderCode c
renderEntry (Image i a) = renderImage i a

renderSection :: String -> [Entry] -> String
renderSection t e =
    hdr ++ t ++ "\",\"content\":" ++ renderList e ++ "}}"
  where
    hdr = "{\"section\":{\"title\":\""

renderListItem :: Entry -> String
renderListItem e = '[' : renderEntry e ++ "]"

renderListContent :: [Entry] -> String
renderListContent [] = "[]"
renderListContent (x : xs) = foldr app (renderListItem x) lst
  where
    lst = map renderListItem xs

renderListEntry :: [Entry] -> String
renderListEntry e = "{\"list\":" ++ renderListContent e ++ "}"

renderLink :: String -> [Entry] -> String
renderLink ul at =
    "{\"link\":{\"url\":\""
        ++ ul
        ++ "\",\"content\":["
        ++ renderList at
        ++ "]}}"

renderCodeBlock :: [Entry] -> String
renderCodeBlock e = "{\"codeblock\":[" ++ renderList e ++ "]}"

renderBold :: Entry -> String
renderBold e = "{\"bold\":" ++ renderEntry e ++ "}"

renderItalic :: Entry -> String
renderItalic e = "{\"italic\":" ++ renderEntry e ++ "}"

renderCode :: Entry -> String
renderCode e = "{\"code\":" ++ renderEntry e ++ "}"

renderImage :: String -> [Entry] -> String
renderImage ul at =
    "{\"image\":{\"url\":\""
        ++ ul
        ++ "\",\"alt\":["
        ++ renderList at
        ++ "]}}"
