{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- JsonDisplayer
-}

module Json (renderJson, renderAuthor, renderTitle, renderDate, renderHeader, renderBody, renderEntry) where

import Document (Document (..), Entry (..), Header (..))
import System.IO (Handle, hPutStr, hPutStrLn)

renderJson :: Document -> String
renderJson (Document hdr bd) = '{' : (renderHeader hdr) ++ (renderBody bd) ++ "}"

renderHeader :: Header -> String
renderHeader (Header t a d) =
    "\"header\":{" ++ (renderTitle t) ++ (renderAuthor a) ++ (renderDate d) ++ "}"

renderAuthor :: Maybe String -> String
renderAuthor (Just auth) = ",\"author\":\"" ++ auth ++ "\""
renderAuthor Nothing = ""

renderTitle :: String -> String
renderTitle t = "\"title\":\"" ++ t ++ "\""

renderDate :: Maybe String -> String
renderDate (Just d) = ",\"date\":\"" ++ d ++ "\""
renderDate Nothing = ""

append :: String -> String -> String
append s res = res ++ ',' : s

renderBody :: [Entry] -> String
renderBody e = ",\"body\":[" ++ (renderList e) ++ "]"

renderList :: [Entry] -> String
renderList [] = ""
renderList (x : xs) = (foldr append (renderEntry x) (map renderEntry xs))

renderEntry :: Entry -> String
renderEntry (Text s) = '"' : s ++ "\""
renderEntry (Paragraph p) = '[' : (renderList p) ++ "]"
renderEntry (Section t c) = renderSection t c
renderEntry (List e) = renderListEntry e
renderEntry (Link ul at) = renderLink ul at
renderEntry (CodeBlock c) = renderCodeBlock c
renderEntry _ = "null"

renderSection :: String -> [Entry] -> String
renderSection t e = "{\"title\":\"" ++ t ++ "\",\"content\":" ++ renderList e ++ "}"

renderListItem :: Entry -> String
renderListItem e = '[' : (renderEntry e) ++ "]"

renderListContent :: [Entry] -> String
renderListContent [] = ""
renderListContent (x : xs) = (foldr append (renderListItem x) (map renderListItem xs))

renderListEntry :: [Entry] -> String
renderListEntry e = "{\"list\":[" ++ (renderListContent e) ++ "]}"

renderLink :: Entry -> [Entry] -> String
renderLink ul at = "{\"url\":" ++ renderEntry ul ++ ",\"content\":" ++ renderList at ++ "}"

renderCodeBlock :: [Entry] -> String
renderCodeBlock e = "{\"list\":[" ++ (renderList e) ++ "]}"

