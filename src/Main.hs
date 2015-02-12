module Main where

import Humus.Card

main :: IO ()
main = putStrLn $ show $ deckToCurve $ [Card "Fake card" [Blue, Black] [ManaSymbol Black, ManaSymbol Blue, ManaSymbol Black] 5 [Beta], Card "Fake card2" [Green, Black] [ManaSymbol Black, ManaSymbol Green] 5 [Beta]]
