{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Spec
-}

import Test.Hspec

import ParserSpec

main :: IO ()
main = hspec $ do
    describe "Parser" ParserSpec.spec