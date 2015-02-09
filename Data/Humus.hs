module Data.Humus (Card(..), Color(..), ManaCost(..), Printing(..), fromName) where

data Color = Blue | Black | Red | White | Green deriving (Show, Eq)
data ManaCost = Colorless Int | Colored Color deriving (Show, Eq)
data Printing = Alpha | Beta deriving (Show, Eq) --Obviously needs more members.
data Card = Card {  name :: String,
                    colors :: [Color],
                    manaCost :: [ManaCost], 
                    sets :: [Printing]    -- All sets the card was printed in
                  } deriving (Show, Eq)




type Error = String -- Experience has taught me this is a bad idea, but we'll go with it for now.

fromName :: String -> Either Error Card
fromName = error "Not yet implemented"

