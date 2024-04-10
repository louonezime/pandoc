{-
-- EPITECH PROJECT, 2024
-- pandoc
-- File description:
-- File
-}

module File () where

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

getInput :: Maybe String -> IO (String)
getInput Nothing = exitWith (ExitFailure 84) >> return ""
getInput (Just p) =
    catch
        (readFile p)
        ( \e ->
            hPutStrLn stderr (readError e p)
                >> exitWith (ExitFailure 84)
                >> return ""
        )

getOutput :: Maybe String -> IO (Handle)
getOutput Nothing = return stdout
getOutput (Just s) = openFile s WriteMode
