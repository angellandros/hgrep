module Main where

import System.Environment

readWords arg = do
 f <- readFile arg
 let w = words f
 -- let count = show (length w)
 -- putStr count
 printWords w


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
