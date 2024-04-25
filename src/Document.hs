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
        { bold :: Entry
        }
    | Italic
        { italic :: Entry
        }
    | Code
        { code :: Entry
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
          alt :: Entry
        }
    | Image
        { url :: String,
          alt :: Entry
        }
    | Paragraph
        { content :: [Entry]
        }
    deriving (Show)

data Document = Document
    { header :: Header,
      body :: [Entry]
    }
    deriving (Show)
