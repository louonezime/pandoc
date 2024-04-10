{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Lib
-}

module Document () where

data Header = Header
    { title :: String,
      author :: String,
      date :: String
    }
    deriving (Show)

data Entry
    = Text
        { txt :: String
        }
    | Bold
        { text :: Entry
        }
    | Italic
        { text :: Entry
        }
    | Code
        { text :: Entry
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
        { url :: Entry,
          content :: [Entry]
        }
    | Image
        { url :: Entry,
          alt :: [Entry]
        }
    | Paragraph
        { content :: [Entry]
        }
    deriving (Show)

data Document = Document
    { header :: Header,
      body :: [Entry]
    }


