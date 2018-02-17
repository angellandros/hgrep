module Main where

import System.Environment
import Grammar

-- reads the file, send the lines to checkLine
readWords arg = do
 f <- readFile arg
 let l = lines f
 checkLine l
 -- let m = map words l
 -- let w = Grammar.match . (map words l)
 -- let count = show (length w)
 -- putStr count
 -- map Grammar.match w regex
 -- printWords w
 -- printWords l
 -- mapM printWords m

-- gets the list of lines of a file and check if the contents match or not
-- checkLine [] regex = putStr ""
checkLine [x] regex = do 
    let ws = words x
    let isMatched = orList (matchLine ws)
    -- let isMatched = map (Grammar.match (words x)) (Grammar.str2regex regex)
    -- let result = orList isMatched
    if (isMatched) then putStrLn x else putStr ""
checkLine (x:xs) regex = checkLine x

-- gets a single line and check the matching situation
matchLine [] regex = False
matchLine [x] regex = Grammar.match x (Grammar.str2regex regex)
matchLine (x:xs) regex = matchLine x regex ++ matchLine xs regexs
-- wordsOfLine [] = putStr ""
-- wordsOfLine x = do
--     let ws = words x
--     let strRegex = Grammar.str2regex regex
--     let foundList = 

-- gets a list of bools and do the OR between the elements
orList [] = False
orList [x] = x
orList (x:xs) = x || orList xs

-- gets the list of Strings and prints them
printWords [] = putStr ""
printWords [x] = putStrLn x
printWords (x:xs) = do 
    let element = x
    putStrLn element
    printWords xs


main = do 
    (regex:files) <- getArgs

    putStrLn regex -- just to show the first arg
    mapM readWords files
