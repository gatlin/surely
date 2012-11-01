module Main where

import AI.Surely

main :: IO ()
main = do
    putStrLn $ show $ solve [[1,2],[-1,3],[-3]]
    putStrLn $ show $ solve [ [1], [-1] ]
    putStrLn $ show $ solve [[]]
    return ()
