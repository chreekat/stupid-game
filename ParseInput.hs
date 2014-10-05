{-# LANGUAGE OverloadedStrings #-}

module ParseInput (parseInput) where

import Control.Applicative ((<*), (<*>), (<$>), many)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text

import Types

parseInput :: Text -> [GameData]
parseInput t = case parseOnly parseFile t of
    Right r -> r
    _ -> error "Parse failure. Not my problem."

parseFile = do
    trump <- parseTrump
    handPairs <- many1 parseHandPair
    return $ map (gameData trump) handPairs

  where

    gameData trump (h1, h2) = GD trump (P h1 1) (P h2 2) []

-- "The trump suite is represented as the first line in data file by a
-- letter: H|D|C|S."
parseTrump = parseSuit <* endOfLine

-- "Two players get their cards which are represented by a line in the
-- file: ST D2 CJ HQ DA | H2 D3 C4 S5 H6 D7 C8 S9"
parseHandPair = (,)
    <$> sepBy parseCard " " <* parseSep
    <*> sepBy parseCard " " <* endOfLine

parseSep = " | "

parseCard = Card <$> parseSuit <*> parseRank

parseSuit :: Parser Suit
parseSuit = magicParse "HDCS"

parseRank :: Parser Rank
parseRank =  magicParse "123456789TJKQA"

-- | Magically parses a value of type 'b' that has Enum and Bounded
-- instances from a single character that represents that value (listed as a
-- String of possibilities in 'keys').
magicParse :: (Bounded b, Enum b) => String -> Parser b
magicParse keys =
    let pairs = zip keys [minBound..]
    in do
        x <- satisfy $ inClass keys
        return . fromJust $ lookup x pairs
