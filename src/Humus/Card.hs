module Humus.Card (Card(..),
    Color(..),
    ManaSymbol(..),
    Printing(..),
    Deck,
    Curve,
    fromName,
    deckToCurve,
    countManaSymbols,
    deckToAvgCMC) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

--Used for mana symbols and also the color identity of the card.
data Color = Blue | Black | Red | White | Green deriving (Show, Eq, Ord)
--Represents the colored mana symbols on the cards.
newtype ManaSymbol = ManaSymbol { unManaSymbol :: Color } deriving (Eq, Show)
data Printing = Alpha | Beta deriving (Show, Eq) --Obviously needs more members.
data Card = Card {  name :: String,
                    colorIdentity :: [Color],
                    manaSymbols :: [ManaSymbol], 
                    cmc :: Int,
                    sets :: [Printing]    -- All sets the card was printed in
                  } deriving (Show, Eq)




type Error = String -- Experience has taught me this is a bad idea, but we'll go with it for now.
type Deck = [Card]
--Map of Cost -> Map (Color -> number of cards)
type Curve = Map Int (Map Color Int)

--How many of a particular manasymbol exists on the card.
countManaSymbols :: ManaSymbol -> Card -> Int
countManaSymbols m = length . filter (/= m) . manaSymbols 

deckToCurve :: Deck -> Curve
deckToCurve = foldl' (Map.unionWith(sum2Maps))(Map.empty) . fmap cardToCurve

deckToAvgCMC :: Deck -> Float
deckToAvgCMC deck = 60 / ((foldl (+) 0.0) . fmap (fromIntegral . cmc)) deck

--Convert a single card into a curve for that Card
cardToCurve :: Card -> Curve
cardToCurve (Card _ _ m cost _) = Map.singleton cost (sumMaps $ fmap ((flip Map.singleton 1) . unManaSymbol) m)


--2 general functions for summing up maps. These need to be moved.
sumMaps :: (Ord k, Num a) => [Map k a] -> Map k a
sumMaps = foldl' (Map.unionWith(+))(Map.empty)

sum2Maps :: (Ord k, Num a) => Map k a -> Map k a -> Map k a
sum2Maps first second = sumMaps [first, second]


--Parsing stuff goes below here
fromName :: String -> Either Error Card
fromName = error "Not yet implemented"
