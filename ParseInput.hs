{-# LANGUAGE OverloadedStrings #-}

module ParseInput where

import Control.Applicative ((<*), (<*>), (<$>), many)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
-- Since this is only an exercise in Free/FreeT, I am punting on doing the
-- right thing and tokenizing the input. attoparsec for the (quick-n-dirty)
-- win.
import Data.Attoparsec.Text

import Types

parseInput :: Text -> [GameData]
parseInput t = case parse parseFile t of
    Done _ r -> r
    Partial f -> case f "" of
        Done _ r -> r
        _ -> error "Parse failure. Not my problem."

parseFile = do
    trump <- parseTrump
    handPairs <- sepBy parseHandPair endOfLine
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
    <*> sepBy parseCard " "

parseSep = " | "

parseCard = Card <$> parseSuit <*> parseRank

parseSuit :: Parser Suit
parseSuit = magicParse "HDCS"

parseRank :: Parser Rank
parseRank =  magicParse "123456789TJQKA"

-- | Magically parses a value of type 'b' that has Enum and Bounded
-- instances from a single character that represents that value (listed as a
-- String of possibilities in 'keys').
magicParse :: (Bounded b, Enum b) => String -> Parser b
magicParse keys =
    let pairs = zip keys [minBound..]
    in do
        x <- satisfy $ inClass keys
        return . fromJust $ lookup x pairs
