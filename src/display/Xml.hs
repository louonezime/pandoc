module Xml (
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
    renderCodeBlock
) where

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

renderBody :: [Entry] -> String
renderBody arr = "<body>" ++ renderElems arr ++ "</body>"

renderElems :: [Entry] -> String
renderElems [] = ""
renderElems (x : xs) = renderElem x ++ renderElems xs

renderBold :: Entry -> String
renderBold (Bold s) = "<bold>" ++ s ++ "</bold>"

renderItalic :: Entry -> String
renderItalic (Italic s) = "<italic>" ++ s ++ "</italic>"

renderCode :: Entry -> String
renderCode (Code s) = "<code>" ++ s ++ "</code>"

renderList :: Entry -> String
renderList (List arr) = "<list>" ++ renderElems arr ++ "</list>"

renderParagraph :: Entry -> String
renderParagraph (Paragraph arr) = "<paragraph>" ++ renderElems arr ++ "</paragraph>"

renderImage :: Entry -> String
renderImage (Image l t) = "<image url=\"" ++ l ++ "\">" ++ t ++ "</image>"

renderLink :: Entry -> String
renderLink (Link l t) = "<link url=\"" ++ l ++ "\">" ++ t ++ "</link>"

renderSection :: Entry -> String
renderSection (Section t arr) = "<section title=\"" ++ t ++ "\">" ++ renderElems arr ++ "</section>"

renderCodeBlock :: Entry -> String
renderCodeBlock (CodeBlock arr) = "<codeblock>" ++ renderElems arr ++ "</codeblock>"

renderElem :: Entry -> String
renderElem (Text t) = t
renderElem x@(Bold _) = renderBold x
renderElem x@(Italic _) = renderItalic x
renderElem x@(Code _) = renderCode x
renderElem x@(List _) = renderList x
renderElem x@(Paragraph _) = renderParagraph x
renderElem x@(Link _ _) = renderLink x
renderElem x@(Image _ _) = renderImage x
renderElem x@(Section _ _) = renderSection x
renderElem x@(CodeBlock _) = renderCodeBlock x

