{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Lib
-}

module Document (Header (..), Entry (..), Document (..)) where

data Header = Header
    { title :: String,
      author :: Maybe String,
      date :: Maybe String
    }
    deriving (Show)

data Entry
    = Text
        { txt :: String
        }
    | Bold
        { bold :: String
        }
    | Italic
        { italic :: String
        }
    | Code
        { code :: String
        }
    | Section
        { sectionTitle :: String,
          content :: [Entry]
        }
    | CodeBlock
        { content :: [Entry]
        }
    | List
        { content :: [Entry]
        }
    | Link
        { url :: String,
          content :: String
        }
    | Image
        { url :: String,
          alt :: String
        }
    | Paragraph
        { content :: [Entry]
        }
    deriving (Show)

data Document = Document
    { header :: Header,
      body :: [Entry]
    }
