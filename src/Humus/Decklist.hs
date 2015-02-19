module Humus.Decklist(readDecklist) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString(breakSubstring)
import Data.Traversable(traverse)


type Error = String
type CardName = String
type ParsedLine = (Int, CardName)
-- Constants
newLineChar :: Char
newLineChar = '\n'
splitLineChar :: Char
splitLineChar = ' '


readDecklist :: B.ByteString -> Either Error [CardName]
readDecklist = fmap ourReplicate . readLines
            where ourReplicate :: [ParsedLine] -> [String] 
                  ourReplicate l = l >>= lineToCardNames

lineToCardNames :: ParsedLine -> [String]
lineToCardNames (n, c) = replicate n c

--Either an error of the (Int, Card Name)
readLines :: B.ByteString -> Either Error [ParsedLine]
readLines b =  traverse readLine  (filter (/= B.empty) (Char8.split newLineChar b))

--Read a line from the file splitting on space and consuming spaces from the front of the 2nd element.
readLine :: B.ByteString -> Either Error ParsedLine
readLine b = let (first, rest) = fmap (Char8.dropWhile (== splitLineChar)) $ breakSubstring (Char8.pack [splitLineChar]) b
             in fmap (\x -> (x, Char8.unpack rest)) (errorOrNumber first)


--Parse a byteString into either an error or an int
errorOrNumber :: B.ByteString -> Either Error Int
errorOrNumber s = check $ (Char8.readInt s)
            where check :: Maybe (Int, Char8.ByteString) -> Either String Int
                  check (Just (i, _))  = Right i
                  check Nothing = Left "Couldn't read Int"
