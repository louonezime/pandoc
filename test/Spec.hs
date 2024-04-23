{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Spec
-}

import Test.Hspec

import JsonDisplaySpec
import ParserSpec
import XmlDisplaySpec

main :: IO ()
main = hspec $ do
    describe "Parser" ParserSpec.spec
    describe "JsonDisplay" JsonDisplaySpec.spec
    describe "xml display" XmlDisplaySpec.spec
