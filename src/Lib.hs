{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Lib
-}

module Lib (Options, options, defaultOptions) where

data Format = Markdown | JSON | XML deriving (Show, Enum)

formatFromString :: String -> Either String Format
formatFromString "markdown" = Right Markdown
formatFromString "xml" = Right XML
formatFromString "json" = Right JSON
formatFromString fmt = Left (fmt++": unrecognized format.")

data Options = Options
    { output :: Maybe String,
      input :: Maybe String,
      format :: Either String Format,
      oformat :: Either String Format
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { output = Nothing,
          input = Nothing,
          format = Left "No format specified.",
          oformat = Left "No format specified."
        }

options :: Options -> [String] -> Either String Options
options opt [] = Right opt
options opt ("-o" : out : xs) = options opt {output = Just out} xs
options opt ("-i" : inp : xs) = options opt {input = Just inp} xs
options opt ("-f": fmt : xs) = options opt {format = (formatFromString fmt)} xs
options opt ("-e": fmt : xs) = options opt {oformat = (formatFromString fmt)} xs
options _ ("--help":_) = Left help
options _ (opt:_) = Left (opt++": unrecognized option see --help.")

help :: String
help = "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\n\n\
    \   -i      input file\n\
    \   -f      output format\n\
    \   -o      output file\n\
    \   -e      input format"
