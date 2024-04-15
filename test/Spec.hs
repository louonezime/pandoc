{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Spec
-}

import Test.Hspec

import ParserSpec
import JsonDisplaySpec

main :: IO ()
main = hspec $ do
    describe "Parser" ParserSpec.spec
    describe "JsonDisplay" JsonDisplaySpec.spec 

