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
    args <- getArgs
    mapM readWords args
