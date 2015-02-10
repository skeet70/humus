module Data.Humus (Card(..), 
    Color(..), 
    ManaCost(..), 
    Printing(..), 
    fromName,
    manaCostToInt,
    cardsToCurve) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable(foldMap)

data Color = Blue | Black | Red | White | Green deriving (Show, Eq, Ord)
data ManaCost = Colorless Int | Colored Color deriving (Show, Eq)
data Printing = Alpha | Beta deriving (Show, Eq) --Obviously needs more members.
data Card = Card {  name :: String,
                    colors :: [Color],
                    manaCost :: [ManaCost], 
                    sets :: [Printing]    -- All sets the card was printed in
                  } deriving (Show, Eq)




type Error = String -- Experience has taught me this is a bad idea, but we'll go with it for now.
type Deck = [Card]
--Map of Cost -> Map (Color -> number of cards)
type Curve = Map Int (Map Color Int)


fromName :: String -> Either Error Card
fromName = error "Not yet implemented"

manaCostToInt :: ManaCost -> Int
manaCostToInt (Colorless n) = n
manaCostToInt _ = 1

cardsToCurve :: Deck -> Curve
cardsToCurve = foldMap cardToCurve

cardToCurve :: Card -> Curve
cardToCurve (Card _ myColors cost _) = Map.singleton (sum $ fmap manaCostToInt cost) (foldMap (flip Map.singleton 1) myColors)
