module Main where

import Data.Humus

main :: IO () 
main = putStrLn $ show $ cardsToCurve $ [Card "Fake card" [Blue, Black] [Colorless 2, Colored Black, Colored Blue] [Beta]]
