{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (Handle, hPutStrLn, stderr)

import Lib (Options (Options), defaultOptions, help, options, runPandoc)

import File (getInput, getOutput)

displayResult :: Handle -> Either String String -> IO ()
displayResult _ (Left e) = hPutStrLn stderr e >> exitWith (ExitFailure 84)
displayResult h (Right s) = hPutStrLn h s

prepareAndRun :: Options -> IO ()
prepareAndRun (Options o (Just i) (Right ifmt) (Right ofmt)) = do
    inpt <- getInput i
    handle <- getOutput o
    displayResult handle (runPandoc ifmt ofmt inpt)

checkErrors :: Either String Options -> IO ()
checkErrors (Right (Options _ Nothing _ _)) = hPutStrLn stderr help >> exitWith (ExitFailure 84)
checkErrors (Right (Options _ _ (Left e) _)) = hPutStrLn stderr e >> exitWith (ExitFailure 84)
checkErrors (Right (Options _ _ _ (Left e))) = hPutStrLn stderr e >> exitWith (ExitFailure 84)
checkErrors (Right o) = prepareAndRun o
checkErrors (Left s) = hPutStrLn stderr s >> exitWith (ExitFailure 84)

main :: IO ()
main = getArgs >>= checkErrors . options defaultOptions
