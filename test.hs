module Main where

import AI.Surely

main :: IO ()
main = mapM_ (print . solve) [ [[1,2],[-1,3],[-3]], [ [1], [-1] ], [[]] ]
