{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Parser
-}

module ParserSpec (spec) where

import Control.Applicative (Alternative (..))
import Test.Hspec

import Parsing

spec :: Spec
spec = do
    describe "parseChar" $ do
        context "when a char that is in string" $ do
            it "returns a tuple of the char and the rest of string" $
                runParser (parseChar 'a' <|> parseChar 'b') "abcd" `shouldBe` Right ('a', "bcd")
