{-# LANGUAGE DeriveFunctor #-}

module DSL where

import Control.Monad.Trans.Free (FreeT(..), liftF)

import Types
import GameState as GS

data GameAction nxt
    = SmallestCard Role (Maybe Card) (Maybe Card -> nxt)
    | PlayCard Role Card nxt
    | AttackRank Rank (Rank -> nxt)
    deriving (Functor)

type GameDSL = FreeT GameAction GameState

attackRank = undefined
cardsRanked = undefined
coverCard = undefined
getHand = undefined
handSize = undefined
playCard = undefined
possibleReinforcements = undefined
reinforceWith = undefined
smallestCard = undefined
swapRoles = undefined
tableSize :: GameDSL Int
tableSize = undefined
tieGame = undefined
uncoveredCards = undefined
winner = undefined
winTurn = undefined
