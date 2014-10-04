{-# LANGUAGE OverloadedStrings #-}

module ParseInput (parseInput) where

import Control.Applicative ((<*), (<*>), (<$>), many)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text

import Types

parseInput :: Text -> [GameState]
parseInput t = case parseOnly parseFile t of
    Right r -> r
    _ -> error "Parse failure. Not my problem."

parseFile = do
    trump <- parseTrump
    handPairs <- many1 parseHandPair
    return $ map (gameState trump) handPairs

  where

    gameState trump (h1, h2) = GS trump h1 h2 []

-- "The trump suite is represented as the first line in data file by a
-- letter: H|D|C|S."
parseTrump = do
    x <- parseSuit
    endOfLine
    return x

-- "Two players get their cards which are represented by a line in the
-- file: ST D2 CJ HQ DA | H2 D3 C4 S5 H6 D7 C8 S9"
parseHandPair = (,)
    <$> sepBy parseCard " " <* parseSep
    <*> sepBy parseCard " " <* endOfLine

parseSep = " | "

parseCard = Card <$> parseSuit <*> parseRank

parseSuit = do
    x <- satisfy $ inClass "HDCS"
    return $ case x of
        'H' -> Heart
        'D' -> Diamond
        'C' -> Club
        'S' -> Spade

parseRank = do
    x <- satisfy $ inClass "123456789TJKQA"
    return $ case x of
        '1' -> R1
        '2' -> R2
        '3' -> R3
        '4' -> R4
        '5' -> R5
        '6' -> R6
        '7' -> R7
        '8' -> R8
        '9' -> R9
        'T' -> RT
        'J' -> RJ
        'K' -> RK
        'Q' -> RQ
        'A' -> RA
