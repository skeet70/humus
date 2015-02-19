module Main where

import Humus.Card()
import Humus.CardDatabase(readDatabaseFromFile)
import Humus.Decklist(readDecklist)
import Humus.Card(Card(..), deckToCurve, Curve)
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Control.Monad.Trans.Either as E
import Data.Maybe(fromMaybe)
import System.Environment

data Arguments = Arguments{
    deckPath :: String,
    cardDBPath :: Maybe String
}
{-
    Really rough main that just prints the CMC of the deck.
    Usage - <deckFilename> <cardDBFilename>
-}
main :: IO ()
main = do
    args <- getArgs
    parsedArgs <- return (parseArgs args)
    result <- convertResult $ E.hoistEither parsedArgs >>= myMainAsEitherT 
    putStrLn result

--TODO do something better with errors
convertResult :: (Show a, Show e) => E.EitherT e IO a -> IO String
convertResult = fmap (either show show) . E.runEitherT

parseArgs :: [String] -> Either String Arguments
parseArgs [deckFilename, cardDBFilename] = Right $ Arguments deckFilename (Just cardDBFilename)
parseArgs [deckFilename] = Right $ Arguments deckFilename Nothing
parseArgs _ = Left "Wrong args, pass <deckFilename> <cardDBFilename>"

myMainAsEitherT:: Arguments ->  E.EitherT String IO Curve
myMainAsEitherT (Arguments deckFilename cardDBFilename) = do 
     eitherErrorOrCardMap <- readJsonCards  $ fromMaybe "data/AllCards-x.json" cardDBFilename
     eitherErrorOrCardList <- readDecklistFromFile deckFilename
     deck <- (E.hoistEither . T.traverse (noteError "Error: Card not found." . flip M.lookup eitherErrorOrCardMap)) eitherErrorOrCardList
     return $ deckToCurve deck
     --TODO make the error message more exact. What card name?

--Returned [String] are supposed card names. Maybe a newtype is in order? 
readDecklistFromFile :: FilePath -> E.EitherT String IO [String]
readDecklistFromFile = E.EitherT . fmap readDecklist . B.readFile

--Either an Error or the Map of cards from our input file
readJsonCards :: FilePath -> E.EitherT String IO (M.Map String Card)
readJsonCards = E.EitherT . fmap (fmap fst) . readDatabaseFromFile

--[String] is a list of "card names" we want to look up
--Look up the supposed card names or return an error
cardNamesToCards :: [String] -> Map String Card -> Either String [Card] 
cardNamesToCards list m = T.traverse (\k -> (noteError ("tried to find " ++ show k) . M.lookup k) m) list

--Utility function to add an error e as the Left of an either.
--This should move to core or something?
noteError ::  e -> Maybe a -> Either e a
noteError _ (Just a) = Right a
noteError e Nothing = Left e
