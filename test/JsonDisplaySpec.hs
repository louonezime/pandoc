{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Parser
-}

module JsonDisplaySpec (spec) where

import Test.Hspec

import Document (Entry (..), Header (..))
import Display.Json

spec :: Spec
spec = do
    describe "JsonDisplaySpec" $ do
        it "display title wrapped in a json key" $ do
            renderTitle "Je connais cette theorie"
                `shouldBe` "\"title\":\"Je connais cette theorie\""

        it "display author wrapped in a json key if author is just" $ do
            renderAuthor (Just "Hubert Bonisseur de La Bath")
                `shouldBe` ",\"author\":\"Hubert Bonisseur de La Bath\""

        it "dont display anything if author is nothing" $ do
            renderAuthor Nothing `shouldBe` ""

        it "display date wrapped in a json key if date is just" $ do
            renderDate (Just "2006-04-19") `shouldBe` ",\"date\":\"2006-04-19\""

        it "dont display anything if date is nothing" $ do
            renderDate Nothing `shouldBe` ""

        it "full header test" $ do
            renderHeader (Header "Tout les allemands ne sont pas nazi" Nothing Nothing)
                `shouldBe` "\"header\":{\"title\":\"Tout les allemands ne sont pas nazi\"}"

        it "empty body" $ do
            renderBody [] `shouldBe` ",\"body\":[]"

        it "render simple text" $ do
            renderEntry (Text "C'est une bonne situation ca la tek 1") `shouldBe` "\"C'est une bonne situation ca la tek 1\""

        it "render paragraph" $ do
            renderEntry (Paragraph [(Text "Hello"), (Text "World")]) `shouldBe` "[\"Hello\",\"World\"]"

        it "render List" $ do
            renderEntry (List [(Paragraph [(Text "Vous etez odile de ray")])]) `shouldBe` "{\"list\":[[\"Vous etez odile de ray\"]]}"

        it "render codeblock" $ do
            renderEntry (CodeBlock [(Paragraph [(Text "int main (void)")])]) `shouldBe` "{\"codeblock\":[[\"int main (void)\"]]}"

        -- it "render link" $ do
        --     renderEntry (Link ("https://perdu.com") [Text "perdu"]) `shouldBe` "{\"link\":{\"url\":\"https://perdu.com\",\"content\":[\"perdu\"]}}"

        -- it "render image" $ do
        --     renderEntry (Image ("https://perdu.com") [Text "perdu"]) `shouldBe` "{\"image\":{\"url\":\"https://perdu.com\",\"alt\":[\"perdu\"]}}"

        it "render bold" $ do
            renderEntry (Bold (Text "Hello")) `shouldBe` "{\"bold\":\"Hello\"}"

        it "render italic" $ do
            renderEntry (Italic (Text "Hello")) `shouldBe` "{\"italic\":\"Hello\"}"

        it "render code" $ do
            renderEntry (Code (Text "Hello")) `shouldBe` "{\"code\":\"Hello\"}"

        it "render section" $ do
            renderEntry (Section "title" []) `shouldBe` "{\"section\":{\"title\":\"title\",\"content\":[]}}"
