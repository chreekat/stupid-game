module Types where

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

data Suit = Heart | Diamond | Club | Spade
    deriving (Bounded, Enum)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA
    deriving (Bounded, Enum)

instance Show Player where
    show p = "P" ++ (show $ pid p) ++ ":" ++ show (hand p)

instance Show Suit where
    show s = case s of
        Heart -> "♥"
        Diamond -> "♦"
        Club -> "♣"
        Spade -> "♠"

instance Show Rank where
    show r = case r of
        R1 -> "1"
        R2 -> "2"
        R3 -> "3"
        R4 -> "4"
        R5 -> "5"
        R6 -> "6"
        R7 -> "7"
        R8 -> "8"
        R9 -> "9"
        RT -> "T"
        RJ -> "J"
        RQ -> "Q"
        RK -> "K"
        RA -> "A"

instance Show Card where
    show (Card s r) = show s ++ show r

instance Show PlayedCard where
    show (PC ca co) = case co of
        Just co' -> show ca ++ "/" ++ show co'
        Nothing  -> show ca ++ "/-"
