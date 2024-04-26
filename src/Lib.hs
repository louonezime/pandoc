{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Lib
-}

module Lib (
    Options (Options),
    optionsParser,
    defaultOptions,
    runPandoc,
    help,
    Format,
) where

import Data.List (isSuffixOf)
import Display.Json (renderJson)
import Display.Markdown (renderMarkdown)
import Display.Xml (renderXml)
import Document (Document (..))
import Parser.Markdown (parseMarkdown)
import Parsing (Parser (..))

data Format = Markdown | JSON | XML deriving (Show, Enum)

formatFromString :: String -> Either String Format
formatFromString "markdown" = Right Markdown
formatFromString "xml" = Right XML
formatFromString "json" = Right JSON
formatFromString fmt = Left (fmt ++ ": unrecognized format.")

data Options = Options
    { output :: Maybe String,
      input :: Maybe String,
      iformat :: Either String Format,
      oformat :: Either String Format
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { output = Nothing,
          input = Nothing,
          iformat = Left "No input format specified.",
          oformat = Left "No output format specified."
        }

optionsParser :: Options -> [String] -> Either String Options
optionsParser opt [] =
    Right (opt {iformat = getInputFormat (iformat opt) (input opt)})
optionsParser opt ("-o" : out : xs) = optionsParser opt {output = Just out} xs
optionsParser opt ("-i" : inp : xs) = optionsParser opt {input = Just inp} xs
optionsParser opt ("-e" : f : xs) =
    optionsParser opt {iformat = formatFromString f} xs
optionsParser opt ("-f" : f : xs) =
    optionsParser opt {oformat = formatFromString f} xs
optionsParser _ ("--help" : _) = Left help
optionsParser _ (opt : _) = Left (opt ++ ": unrecognized option see --help.")

getInputFormat :: Either String Format -> Maybe String -> Either String Format
getInputFormat (Right f) _ = Right f
getInputFormat (Left _) (Just s)
    | ".md" `isSuffixOf` s = Right Markdown
    | ".xml" `isSuffixOf` s = Right XML
    | ".json" `isSuffixOf` s = Right JSON
getInputFormat _ _ = Left "No format recognized"

help :: String
help =
    "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\n\n\
    \   -i      input file\n\
    \   -f      output format (xml, markdown, json)\n\
    \   -o      output file\n\
    \   -e      input format (xml, markdown, json)"

dumpPandoc :: Format -> Document -> String
dumpPandoc XML = renderXml
dumpPandoc JSON = renderJson
dumpPandoc Markdown = renderMarkdown

parseDocument :: Format -> Parser Document
parseDocument Markdown = parseMarkdown
parseDocument _ = Parser $ \_ -> Left "Unimplemented"

--- To test the program change the [] to _

getDocument :: Format -> String -> Either String Document
getDocument f s = case runParser (parseDocument f) s of
    Right (x, _) -> Right x
    Left e -> Left e
    _ -> Left "Bad document given"

runPandoc :: Format -> Format -> String -> Either String String
runPandoc i f s = dumpPandoc f <$> getDocument i s
