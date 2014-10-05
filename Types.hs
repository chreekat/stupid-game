module Types where

import Data.Maybe (fromJust)

data GameData = GD {
    trump :: Suit,
    offense :: Player,
    defense :: Player,
    table :: [PlayedCard]
} deriving (Show)

data Player = P {
  hand :: [Card],
  pid :: Int
}

data PlayedCard = PC {
    card :: Card,
    cover :: Maybe Card
}

data Card = Card Suit Rank
    deriving (Eq)

data Suit = Heart | Diamond | Club | Spade
    deriving (Bounded, Enum, Eq)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
    deriving (Bounded, Enum, Eq)

data Role = Offense | Defense deriving (Show)

--
-- Instances
--

instance Show Player where
    show p = "P" ++ (show $ pid p) ++ ":" ++ show (hand p)

instance Show Suit where
    show = magicShow "♥♦♣♠"

instance Show Rank where
    show = magicShow "123456789TJQKA"

magicShow :: (Bounded b, Enum b, Eq b) => String -> b -> String
magicShow keys val =
    let pairs = zip [minBound..] keys
    in (fromJust $ lookup val pairs) : []

instance Show Card where
    show (Card s r) = show s ++ show r

instance Show PlayedCard where
    show (PC ca co) = case co of
        Just co' -> show ca ++ "/" ++ show co'
        Nothing  -> show ca ++ "/-"
