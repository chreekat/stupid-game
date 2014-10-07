{-# LANGUAGE DeriveFunctor #-}

module DSL where

import Data.List (delete)
import qualified Control.Exception as Ex (assert)
import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Types
import GameState (GameState)
import qualified GameState as GS
import CardFuncs

data GameAction nxt
    = SmallestCard Role (Maybe Card) (Maybe Card -> nxt)
    | PlayCard Role Card nxt
    | TableRank [Rank] ([Rank] -> nxt)
    | CoverCard Card Bool (Bool -> nxt)
    deriving (Functor)

type GameDSL = FreeT GameAction GameState

cardsRanked :: Suit -> Role -> GameDSL [Card]
cardsRanked = undefined
handSize :: Role -> GameDSL Int
handSize = undefined
reinforceWith = undefined
playedCards = undefined
swapRoles = undefined
tableSize :: GameDSL Int
tableSize = undefined
tieGame = undefined
uncoveredCards :: GameDSL [PlayedCard]
uncoveredCards = undefined
winner = undefined
winTurn = undefined
defend = undefined

-- | assertions for the DSL programmer
assert test = return $ Ex.assert test $ Pure ()

playCard :: Role -> Card -> GameDSL ()
playCard role card = do
    hand <- getHand role
    assert (card `elem` hand)
    lift $ GS.updateHand role (delete card)
    lift $ GS.playCard card
    liftF $ PlayCard role card ()

getHand = lift . GS.getHand

getTrump :: GameDSL Suit
getTrump = lift GS.getTrump
