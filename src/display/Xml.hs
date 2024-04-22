module Xml (renderXml, renderHeader, renderTitle, renderAuthor, renderDate) where

import Document (Document (..), Entry (..), Header (..))

renderXml :: Document -> String
renderXml (Document hd bdy) = "<document>" ++ (renderHeader hd) ++ "</document>"

renderTitle :: String -> String
renderTitle s = '"' : s ++ "\""

renderAuthor :: Maybe String -> String
renderAuthor Nothing = ""
renderAuthor (Just s) = "<author>" ++ s ++ "</author>"

renderDate :: Maybe String -> String
renderDate Nothing = ""
renderDate (Just s) = "<date>" ++ s ++ "</date>"

renderHeader :: Header -> String
renderHeader (Header t a d) =
    "<header title="
        ++ renderTitle t
        ++ '>'
        : renderAuthor a
        ++ renderDate d
        ++ "</header>"
