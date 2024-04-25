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
            it "fails if the specified character is not found" $ do
                let result = runParser (parseChar 'a') "def"
                result `shouldBe` Left "a: not found"

    describe "parseAnyChar" $ do
        it "parses a character from the specified string" $ do
            let parser = parseAnyChar "abc"
            let result = runParser parser "bcd"
            result `shouldBe` Right ('b', "cd")

        it "fails if the character is not in the specified string" $ do
            let parser = parseAnyChar "abc"
            let result = runParser parser "def"
            result `shouldBe` Left "abc: not found"

    describe "parseSomeChar" $ do
        it "parses a character from the specified string" $ do
            let parser = parseSomeChar "abc"
            let result = runParser parser "bcd"
            result `shouldBe` Right ('b', "cd")

        it "fails if the character is not in the specified string" $ do
            let parser = parseSomeChar "abc"
            let result = runParser parser "def"
            result `shouldBe` Left "empty"

    describe "parseString" $ do
        it "parses the specified string" $ do
            let result = runParser (parseString "bite") "bite world"
            result `shouldBe` Right ("bite", " world")

        it "fails if the specified string is not found" $ do
            let result = runParser (parseString "bite") "world"
            result `shouldBe` Right ("", "world")

    describe "parseOr" $ do
        it "parses either 'a' or 'b'" $ do
            let parser = parseOr (parseChar 'a') (parseChar 'b')
            let result1 = runParser parser "abc"
            let result2 = runParser parser "bcd"
            result1 `shouldBe` Right ('a', "bc")
            result2 `shouldBe` Right ('b', "cd")

        it "fails if neither 'a' nor 'b' is found" $ do
            let parser = parseOr (parseChar 'a') (parseChar 'b')
            let result = runParser parser "cde"
            result `shouldBe` Left "b: not found"

    describe "parseAnd" $ do
        it "parses 'bite' followed by a space" $ do
            let parser = parseAnd (parseString "bite") (parseChar ' ')
            let result = runParser parser "bite world"
            result `shouldBe` Right (("bite", ' '), "world")

        it "fails if 'bite' is not followed by a space" $ do
            let parser = parseAnd (parseString "bite") (parseChar ' ')
            let result = runParser parser "biteWorld"
            result `shouldBe` Left " : not found"

    describe "parseAndWith" $ do
        it "applies a function to the results of two parsers" $ do
            let parser = parseAndWith (\ x y -> [x, y]) (parseChar 'a') (parseChar 'b')
            let result = runParser parser "abcd"
            result `shouldBe` Right ("ab", "cd")

        it "applies a custom function to the results of two parsers" $ do
            let parser = parseAndWith (\x y -> (x, y)) (parseString "bite") (parseString "world")
            let result = runParser parser "bite world"
            result `shouldBe` Right (("bite", ""), " world")

        it "fails if either parser fails" $ do
            let parser = parseAndWith (+) parseUInt parseUInt
            let result = runParser parser "10 abc"
            result `shouldBe` Left "0123456789: not found"

    describe "parseMany" $ do
        it "parses multiple characters" $ do
            let parser = parseMany (parseAnyChar "abc")
            let result = runParser parser "abcabc"
            result `shouldBe` Right ("abcabc", "")

        it "parses an empty list if no match is found" $ do
            let parser = parseMany (parseChar 'x')
            let result = runParser parser "abcdef"
            result `shouldBe` Right ([], "abcdef")

    describe "parseSome" $ do
        it "parses at least one digit" $ do
            let parser = parseSome (parseCharInStr ['0'..'9'])
            let result = runParser parser "123abc"
            result `shouldBe` Right ("123", "abc")

        it "fails if no match is found" $ do
            let parser = parseSome (parseChar 'x')
            let result = runParser parser "abcdef"
            result `shouldBe` Left "x: not found"

    describe "parseUInt" $ do
        it "parses a valid unsigned integer" $ do
            let parser = parseUInt
            let result = runParser parser "123"
            result `shouldBe` Right (123, "")

        it "fails if no digits are present" $ do
            let parser = parseUInt
            let result = runParser parser "abc"
            result `shouldBe` Left "0123456789: not found"

    describe "parseInt" $ do
        it "parses a positive integer" $ do
            let parser = parseInt
            let result = runParser parser "123"
            result `shouldBe` Right (123, "")

        it "parses a negative integer" $ do
            let parser = parseInt
            let result = runParser parser "-456"
            result `shouldBe` Right (-456, "")

        it "fails if no digits are present" $ do
            let parser = parseInt
            let result = runParser parser "abc"
            result `shouldBe` Left "-: not found"

    describe "parseTuple" $ do
        it "parses a tuple of integers" $ do
            let parser = parseTuple parseInt
            let result = runParser parser "(42,-10)"
            result `shouldBe` Right ((42, -10), "")

        it "fails if the tuple format is incorrect" $ do
            let parser = parseTuple parseInt
            let result = runParser parser "(42,)"
            result `shouldBe` Left "-: not found"

    describe "parseQuotes" $ do
        it "parses a quoted string" $ do
            let result = runParser parseQuotes "\"bite\""
            result `shouldBe` Right ("bite", "")

        it "fails if the quoted string format is incorrect" $ do
            let result = runParser parseQuotes "\"bite"
            result `shouldBe` Left "\": not found"

    describe "parseLine" $ do
        it "parses a line terminated by newline" $ do
            let parser = parseLine
            let result = runParser parser "bite, World!\nRemaining text"
            result `shouldBe` Right ("bite, World!", "Remaining text")

        it "fails if no newline is present" $ do
            let parser = parseLine
            let result = runParser parser "bite, World!"
            result `shouldBe` Left "\n: not found"

    describe "parseNonStr" $ do
        it "parses a character not in the specified string" $ do
            let parser = parseNonStr "aeiou"
            let result = runParser parser "h"
            result `shouldBe` Right ('h', "")

        it "fails if the character is in the specified string" $ do
            let parser = parseNonStr "aeiou"
            let result = runParser parser "e"
            result `shouldBe` Left "aeiou: found"

    describe "parseCharInStr" $ do
        it "parses a character in the specified string" $ do
            let parser = parseCharInStr "aeiou"
            let result = runParser parser "e"
            result `shouldBe` Right ('e', "")

        it "fails if the character is not in the specified string" $ do
            let parser = parseCharInStr "aeiou"
            let result = runParser parser "h"
            result `shouldBe` Left "Expected one of aeiou, but end of input reached"

    describe "parseSeparators" $ do
        it "parses whitespace characters" $ do
            let parser = parseSeparators
            let result = runParser parser " \t\nText"
            result `shouldBe` Right (" \t\n", "Text")

    describe "parseAfter" $ do
        it "parses string after the specified prefix" $ do
            let parser = parseAfter "bite"
            let result = runParser parser "bite, World!"
            result `shouldBe` Right ("bite", ", World!")

        it "fails if the specified prefix is not found" $ do
            let parser = parseAfter "bite"
            let result = runParser parser "Hi, World!"
            result `shouldBe` Left "bite: not prefix"

    describe "parseBefore" $ do
        it "parses characters before a specified substring" $ do
            let result = runParser (parseBefore "world") "bite world"
            result `shouldBe` Right ("bite ", "")

        it "fails if the specified substring is not found" $ do
            let result = runParser (parseBefore "world") "bite"
            result `shouldBe` Left "world: not a suffix"

    describe "parseBetween" $ do
        it "parses text between two strings" $ do
            let parser = parseBetween "<yo>"
            let result = runParser parser "<yo>ContentEnd"
            result `shouldBe` Left ("<yo>: not a suffix")

        it "parses text between two strings" $ do
            let parser = parseBetween "<yo>"
            let result = runParser parser "<yo>ContentEnd<yo>"
            result `shouldBe` Right ("ContentEnd", "")

    describe "parseBetweenTwo" $ do
        it "parses text between two specified strings" $ do
            let parser = parseBetweenTwo "<yo>" "</yo>"
            let result = runParser parser "<yo>content<yo>"
            result `shouldBe` Left ("</yo>: not a suffix")

        it "parses text between two specified strings" $ do
            let parser = parseBetweenTwo "<yo>" "</yo>"
            let result = runParser parser "<yo>content</yo>"
            result `shouldBe` Right ("content", "")

    describe "parseTillEmpty" $ do
        it "parses multiple values until end of input" $ do
            let parser = parseTillEmpty (parseUInt <* parseChar ' ')
            let result = runParser parser "10 20 30 "
            result `shouldBe` Right ([10, 20, 30], "")
