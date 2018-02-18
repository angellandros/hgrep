module Main where

import System.Environment
import Grammar
import Data.Char
import Data.List
import Data.Maybe

-- gets a single line and check the matching situation, returning matched word, before, and after
matchLine' :: [String] -> String -> ([String], String, [String])
matchLine' [] _ = ([], "", [])
matchLine' (w:ws) regexstr 
    | Grammar.match w (Grammar.str2regex regexstr) = ([], w, ws)
    | otherwise = case matchLine' ws regexstr of
        (a, b, c) -> (w:a, b, c)

-- gets a single line and check the matching situation
matchLine :: [String] -> String -> String
matchLine [] _ = []
matchLine (w:ws) regexstr 
    | Grammar.match w (Grammar.str2regex regexstr) = w
    | otherwise = matchLine ws regexstr

-- foreach abstraction
foreach f [] = return ()
foreach f (x:xs) = do
    f x
    foreach f xs

-- get a file path and pass the lines to checkLineAndPrint
matchFileLines regexstr path = do
    content <- readFile path
    foreach (checkLineAndPrint regexstr) (lines content)
    return ()

-- check the line words with the regex (using matchLine) and then print it when match
checkLineAndPrint regexstr line =
    let matched = matchLine' (words line) regexstr in
        case matched of 
            (before, word, after) ->
                if word /= "" then
                    putStrLn $ (unwords before) ++ "\x1b[31m\x1b[1m " ++ word ++ " \x1b[0m" ++ (unwords after)
                else return ()

-- main function
main = do 
    args <- getArgs    
    foreach (matchFileLines (head args)) (drop 1 args)

