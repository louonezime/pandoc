{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

import Lib (Options, defaultOptions, options)

checkErrors :: Either String Options -> IO ()
checkErrors (Right o) = print o
checkErrors (Left s) = hPutStrLn stderr s >> exitWith (ExitFailure 84)

main :: IO ()
main = getArgs >>= checkErrors . options defaultOptions
