module Main where

import System.Environment
import Grammar
import Data.Char
import Data.List
import Data.Maybe

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
    if (matchLine (words line) regexstr) /= [] then
        let w = matchLine (words line) regexstr in
        let i = fromJust (findSubstring w line) in
        putStrLn $ ((slice 0 (i-1) line) ++ "\x1b[32m" ++ (slice i (i + (length w)) line) ++ "\x1b[0m" ++ (slice (i + (length w)+1) (i+(length line)) line))
    else return ()


findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 

slice start end = take (end - start + 1) . drop start

-- main function
main = do 
    args <- getArgs    
    foreach (matchFileLines (head args)) (drop 1 args)

