
module Json (renderJson) where 

import Document (Document (..), Header (..))
import System.IO (Handle, hPutStrLn, hPutStr)

renderJson :: Document -> String
renderJson (Document hdr _) = '{':(renderHeader hdr) ++ "}"

renderHeader :: Header -> String
renderHeader (Header t a d) = "\"header\":{" ++ (renderTitle t) ++ (renderAuthor a) ++ (renderDate d) ++"}"

renderAuthor :: Maybe String -> String
renderAuthor (Just auth) = ",\"author\":\"" ++ auth ++ "\""
renderAuthor Nothing = ""

renderTitle :: String -> String
renderTitle t = "\"title\":\""++t++"\""

renderDate :: Maybe String -> String
renderDate (Just date) = ",\"date\":\"" ++ date ++ "\""
renderDate Nothing = ""

