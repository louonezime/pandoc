{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (Handle, hClose, hPutStrLn, stderr)

import Lib (
    Format,
    Options (Options),
    defaultOptions,
    help,
    optionsParser,
    runPandoc,
 )

import File (getInput, getOutput)

displayResult :: Either String String -> Handle -> IO ()
displayResult (Left e) _ = hPutStrLn stderr e >> exitWith (ExitFailure 84)
displayResult (Right s) h = hPutStrLn h s >> hClose h

prepareAndRun :: Maybe String -> String -> Format -> Format -> IO ()
prepareAndRun o i ifmt ofmt =
    getInput i
        >>= (\s -> getOutput o >>= displayResult s) . runPandoc ifmt ofmt

checkErrors :: Either String Options -> IO ()
checkErrors (Right (Options _ Nothing _ _)) =
    hPutStrLn stderr help >> exitWith (ExitFailure 84)
checkErrors (Right (Options _ _ (Left e) _)) =
    hPutStrLn stderr e >> exitWith (ExitFailure 84)
checkErrors (Right (Options _ _ _ (Left e))) =
    hPutStrLn stderr e >> exitWith (ExitFailure 84)
checkErrors (Right (Options o (Just i) (Right ifmt) (Right ofmt))) =
    prepareAndRun o i ifmt ofmt
checkErrors (Left s) = hPutStrLn stderr s >> exitWith (ExitFailure 84)

main :: IO ()
main = getArgs >>= checkErrors . optionsParser defaultOptions
