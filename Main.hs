module Main where

import Data.Humus
import Generator

main :: IO ()
main = putStrLn $ show $ cardsToCurve $ [Card "Fake card" [Blue, Black] [Colorless 2, Colored Black, Colored Blue, Colored Black] 5 [Beta], Card "Fake card2" [Green, Black] [Colorless 3, Colored Black, Colored Green] 5 [Beta]]
