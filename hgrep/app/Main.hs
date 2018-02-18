module Main where

import System.Environment
import Grammar

-- gets a single line and check the matching situation
matchLine :: [String] -> String -> Bool
matchLine [] _ = False
matchLine (w:ws) regexstr = Grammar.match w (Grammar.str2regex regexstr) || matchLine ws regexstr

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
    if matchLine (words line) regexstr then
        putStrLn line
    else return ()

-- main function
main = do 
    args <- getArgs    
    foreach (matchFileLines (head args)) (drop 1 args)

