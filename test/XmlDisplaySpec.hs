module XmlDisplaySpec (spec) where

import Test.Hspec

import Display.Xml
import Document (Document (..), Entry (..), Header (..))

spec :: Spec
spec = do
    describe "XmlDisplaySpec" $ do
        it "simple title" $ do
            renderTitle "Hello" `shouldBe` "\"Hello\""
        it "simple author" $ do
            renderAuthor (Just "lou") `shouldBe` "<author>lou</author>"
        it "simple date" $ do
            renderDate (Just "2024-04-20") `shouldBe` "<date>2024-04-20</date>"
        it "full header" $ do
            renderHeader (Header "Je connais cette theorie" Nothing Nothing)
                `shouldBe` "<header title=\"Je connais cette theorie\"></header>"
        it "bold text" $ do
            renderBold (Bold (Text "Hello")) `shouldBe` "<bold>Hello</bold>"
        it "italic text" $ do
            renderItalic (Italic (Text "Hello")) `shouldBe` "<italic>Hello</italic>"
        it "code text" $ do
            renderCode (Code (Text "Hello")) `shouldBe` "<code>Hello</code>"
        it "paragraph" $ do
            renderParagraph (Paragraph [Text "Hi", Code (Text "Hello")])
                `shouldBe` "<paragraph>Hi<code>Hello</code></paragraph>"
        it "Image" $ do
            renderImage (Image "www.perdu.com" (Text "No Image"))
                `shouldBe` "<image url=\"www.perdu.com\">No Image</image>"
        it "link" $ do
            renderLink (Link "www.perdu.com" (Text "No Image"))
                `shouldBe` "<link url=\"www.perdu.com\">No Image</link>"
        it "List" $ do
            renderList (List [Paragraph [(Text "www.perdu.com")], Paragraph [(Text "No Image")]])
                `shouldBe` "<list><paragraph>www.perdu.com</paragraph><paragraph>No Image</paragraph></list>"
        it "CodeBlock" $ do
            renderCodeBlock (CodeBlock [Paragraph [(Text "www.perdu.com")], Paragraph [(Text "No Image")]])
                `shouldBe` "<codeblock><paragraph>www.perdu.com</paragraph><paragraph>No Image</paragraph></codeblock>"
        it "Section" $ do
            renderSection
                (Section "Title" [Paragraph [(Text "www.perdu.com")], Paragraph [(Text "No Image")]])
                `shouldBe` "<section title=\"Title\"><paragraph>www.perdu.com</paragraph><paragraph>No Image</paragraph></section>"
