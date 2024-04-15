{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Parser
-}

module JsonDisplaySpec (spec) where

import Test.Hspec

import Json
import Document (Header (..))

spec :: Spec
spec = do 
    describe "JsonDisplaySpec" $ do 
        it "display title wrapped in a json key" $ do 
            renderTitle "Je connais cette theorie" `shouldBe` "\"title\":\"Je connais cette theorie\""

        it "display author wrapped in a json key if author is just" $ do
            renderAuthor (Just "Hubert Bonisseur de La Bath") `shouldBe` ",\"author\":\"Hubert Bonisseur de La Bath\""

        it "dont display anything if author is nothing" $ do 
            renderAuthor Nothing `shouldBe` ""

        it "display date wrapped in a json key if date is just" $ do
            renderDate (Just "2006-04-19") `shouldBe` ",\"date\":\"2006-04-19\""

        it "dont display anything if date is nothing" $ do 
            renderDate Nothing `shouldBe` ""

        it "full header test" $ do 
            renderHeader (Header "Tout les allemands ne sont pas nazi" Nothing Nothing) `shouldBe` "\"header\":{\"title\":\"Tout les allemands ne sont pas nazi\"}" 

