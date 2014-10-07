{-# LANGUAGE DeriveFunctor #-}

module DSL where

import Control.Monad.Trans.Free

import Types
import GameState as GS

data GameAction nxt
    = SmallestCard Role (Maybe Card) (Maybe Card -> nxt)
    | PlayCard Role Card nxt
    | AttackRank Rank (Rank -> nxt)
    deriving (Functor)

type GameDSL = FreeT GameAction GameState

cardsRanked :: Rank -> Role -> GameDSL [Card]
cardsRanked = undefined
getHand :: Role -> GameDSL [Card]
getHand = undefined
getTrump = undefined
handSize :: Role -> GameDSL Int
handSize = undefined
playCard = undefined
reinforceWith = undefined
playedCards = undefined
smallestCard = undefined
swapRoles = undefined
tableSize :: GameDSL Int
tableSize = undefined
tieGame = undefined
uncoveredCards :: GameDSL [PlayedCard]
uncoveredCards = undefined
winner = undefined
winTurn = undefined
defend = undefined
