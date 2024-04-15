{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- JsonDisplayer
-}

module Json (renderJson) where 

import Document (Document (..), Header (..), Entry (..))
import System.IO (Handle, hPutStrLn, hPutStr)

renderJson :: Document -> String
renderJson (Document hdr bd) = '{':(renderHeader hdr) ++ (renderBody bd) ++ "}"

renderHeader :: Header -> String
renderHeader (Header t a d) = "\"header\":{" ++ (renderTitle t) ++ (renderAuthor a) ++ (renderDate d) ++"}"

renderAuthor :: Maybe String -> String
renderAuthor (Just auth) = ",\"author\":\"" ++ auth ++ "\""
renderAuthor Nothing = ""

renderTitle :: String -> String
renderTitle t = "\"title\":\""++t++"\""

renderDate :: Maybe String -> String
renderDate (Just d) = ",\"date\":\"" ++ d ++ "\""
renderDate Nothing = ""

append :: String -> String -> String
append s res = res ++ ',':s

renderBody :: [Entry] -> String
renderBody e = ",\"body\":["++ (renderBodyContent e) ++ "]"

renderBodyContent :: [Entry] -> String
renderBodyContent [] = ""
renderBodyContent (x:xs) = (renderEntry x) ++ (foldr append [] (map renderEntry xs))

renderEntry :: Entry -> String
renderEntry (Text s) = '"':s ++ "\""
renderEntry _ = "null"
