module XmlDisplaySpec (spec) where

import Test.Hspec

import Document (Document (..), Entry (..), Header (..))
import Xml

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
            renderBold (Bold "Hello") `shouldBe` "<bold>Hello</bold>"
        it "italic text" $ do
            renderItalic (Italic "Hello") `shouldBe` "<italic>Hello</italic>"
        it "code text" $ do
            renderCode (Code "Hello") `shouldBe` "<code>Hello</code>"
        it "paragraph" $ do
            renderParagraph (Paragraph [Text "Hi", Code "Hello"])
                `shouldBe` "<paragraph>Hi<code>Hello</code></paragraph>"
        it "Image" $ do
            renderImage (Image "www.perdu.com" "No image")
                `shouldBe` "<image url=\"www.perdu.com\">No Image</image>"
        it "link" $ do
            renderLink (Link "www.perdu.com" "No image")
                `shouldBe` "<link url=\"www.perdu.com\">No Image</link>"
        it "List" $ do
            renderList (List [Paragraph "www.perdu.com", Paragraph "No image"])
                `shouldBe` "<list><paragraph>www.perdu.com</paragraph><paragraph>No Image</paragraph></list>"
        it "CodeBlock" $ do
            renderCodeBlock (CodeBlock [Paragraph "www.perdu.com", Paragraph "No image"])
                `shouldBe` "<list><paragraph>www.perdu.com</paragraph><paragraph>No Image</paragraph></list>"
        it "Section" $ do
            renderSection
                (Section "Title" [Paragraph "www.perdu.com", Paragraph "No image"])
                `shouldBe` "<section title=\"Title\"><paragraph>www.perdu.com</paragraph><paragraph>No Image</paragraph></section>"
