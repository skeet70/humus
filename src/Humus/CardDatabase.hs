{-# LANGUAGE OverloadedStrings #-}
module Humus.CardDatabase (readDatabaseFromFile, readCardsFromDatabase) where

import Data.Aeson
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.List.Split(splitWhen)
import Data.Maybe(maybeToList)
import Humus.Card(Color(..),  ManaSymbol(..), Card(..), Land(..))

type Error = String

data SerdeCard = SerdeCard { _name :: String,
                     _manaSymbols :: [ManaSymbol],
                     _cmc :: Maybe Int,
                     _colors :: [Color],
                     _types :: [String]
                     --legalities :: M.Map String String
                   } deriving (Show, Eq)

instance FromJSON SerdeCard where
 parseJSON (Object v) = SerdeCard <$> v .:  "name"
                                  <*> (manaCostToManaSymbol <$> v .:? "manaCost" .!= "")
                                  <*> v .:? "cmc"
                                  <*> (stringToColor <$> v .:?  "colors" .!= [])
                                  <*> v .:?  "types" .!= []
                                  -- <*> v .: "legalities"

-- Filter out cards that don't have a CMC. This is lands and tokens.
serdeToCard :: SerdeCard -> Maybe Card 
serdeToCard (SerdeCard n m (Just c) cc _) = Just $ Card n m c cc
serdeToCard _ = Nothing

--Converting cards to lands
serdeToLand :: SerdeCard -> Maybe Land
serdeToLand (SerdeCard n _ Nothing _ t) = if elem "Land" t then Just $ Land n "Land" else Nothing
serdeToLand _ = Nothing

--Read cards from the bytestring. The returned maps are from "Card name" -> Card or Land
readCardsFromDatabase :: B.ByteString -> Either Error (M.Map String Card, M.Map String Land)
readCardsFromDatabase bytes = (\m -> ( M.mapMaybe serdeToCard $  m, M.mapMaybe serdeToLand $ m)) <$> decodeSerdeCards bytes
          where decodeSerdeCards :: B.ByteString -> Either String (M.Map String SerdeCard)
                decodeSerdeCards = eitherDecode

--Given a file path produce and an IO action that has both the Card Map and Land Map
--We should consider having this return an EitherT
readDatabaseFromFile :: FilePath ->  IO (Either Error (M.Map String Card, M.Map String Land))
readDatabaseFromFile = fmap readCardsFromDatabase . B.readFile

{-
  Deserialization functions below here.
-}
stringToColor :: [Text.Text] -> [Color]
stringToColor s = wordToColor <$> s
        where wordToColor "Green" = Green
              wordToColor "Black" = Black
              wordToColor "Red" = Red
              wordToColor "White" = White
              wordToColor "Blue" = Blue
              wordToColor x = error ("Found" ++ (show x))

--Flatten the {#} that was in the "manaCost" out of existance.
manaCostToManaSymbol :: Text.Text -> [ManaSymbol]
manaCostToManaSymbol t =  (Data.List.Split.splitWhen (flip elem "{}") (Text.unpack t))  >>= convertElement
    where convertElement x = (maybeToList . symbolStringToManaSymbol) x


symbolStringToManaSymbol :: String -> Maybe ManaSymbol
symbolStringToManaSymbol "U" = Just (ManaSymbol Blue)
symbolStringToManaSymbol "B" = Just (ManaSymbol Black)
symbolStringToManaSymbol "G" = Just (ManaSymbol Green)
symbolStringToManaSymbol "W" = Just (ManaSymbol White)
symbolStringToManaSymbol "R" = Just (ManaSymbol Red)
symbolStringToManaSymbol _ = Nothing



