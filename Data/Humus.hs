module Data.Humus (Card(..),
    Color(..),
    ManaSymbol(..),
    Printing(..),
    fromName,
    cardsToCurve,
    countManaSymbols,
    deckToAvgCMC) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

data Color = Blue | Black | Red | White | Green deriving (Show, Eq, Ord)
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

countManaSymbols :: ManaSymbol -> Card -> Int
countManaSymbols m = length . filter (/= m) . manaSymbols 

fromName :: String -> Either Error Card
fromName = error "Not yet implemented"

manaSymbolToColorMap :: ManaSymbol -> Map Color Int
manaSymbolToColorMap (ManaSymbol c) = Map.singleton c 1

cardsToCurve :: Deck -> Curve
cardsToCurve = foldl' (Map.unionWith(sum2Maps))(Map.empty) . fmap cardToCurve

deckToAvgCMC :: Deck -> Float
deckToAvgCMC deck = 60 / ((foldl (+) 0.0) . fmap (fromIntegral . cmc)) deck

cardToCurve :: Card -> Curve
cardToCurve (Card _ _ m cost _) = Map.singleton cost (sumMaps $ fmap manaSymbolToColorMap m)

sumMaps :: (Ord k, Num a) => [Map k a] -> Map k a
sumMaps = foldl' (Map.unionWith(+))(Map.empty)

sum2Maps :: (Ord k, Num a) => Map k a -> Map k a -> Map k a
sum2Maps first second = sumMaps [first, second]
