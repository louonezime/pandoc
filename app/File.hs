{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- File
-}

module File (getInput, getOutput) where

import Control.Exception (IOException, catch)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (
    Handle,
    IOMode (WriteMode),
    hPutStrLn,
    openFile,
    stderr,
    stdout,
 )

readError :: IOException -> String -> String
readError e p = "Couldn't open " ++ p ++ ": " ++ show e

getInput :: String -> IO String
getInput p =
    catch
        (readFile p)
        ( \e ->
            hPutStrLn stderr (readError e p)
                >> exitWith (ExitFailure 84)
                >> return ""
        )

getOutput :: Maybe String -> IO Handle
getOutput Nothing = return stdout
getOutput (Just s) = openFile s WriteMode
