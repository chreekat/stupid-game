{-# LANGUAGE DeriveFunctor #-}

module DSL where

import Data.List (delete)
import Control.Applicative ((<$>))
import qualified Control.Exception as Ex (assert)
import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Types
import GameState (GameState)
import qualified GameState as GS
import CardFuncs

data GameAction nxt
    = AttackWith Card nxt
    | PassWith Card nxt
    deriving (Functor)

type GameDSL = FreeT GameAction GameState

reinforceWith = undefined
playedCards = undefined
tieGame = undefined
uncoveredCards :: GameDSL [PlayedCard]
uncoveredCards = undefined
winner = undefined
winTurn = undefined
defend = undefined

-- | assertions for the DSL programmer
assert test = return $ Ex.assert test $ Pure ()

attackWith :: Card -> GameDSL ()
attackWith card = do
    playCard Offense card
    liftF $ AttackWith card ()

playCard :: Role -> Card -> GameDSL ()
playCard role card = do
    hand <- getHand role
    assert (card `elem` hand)
    lift $ GS.updateHand role (delete card)
    lift $ GS.playCard card

getHand = lift . GS.getHand

getTrump :: GameDSL Suit
getTrump = lift GS.getTrump

trumpCards role = do
    t <- getTrump
    filter ((== t) . cSuit) <$> getHand role

tableSize :: GameDSL Int
tableSize = length <$> lift (gets table)


cardsRanked :: Rank -> Role -> GameDSL [Card]
cardsRanked r role = do
    hand <- getHand role
    return $ filter ((r ==) . cRank) hand

passWith card = do
    playCard Defense card
    swapRoles
    liftF $ PassWith card id

swapRoles :: GameDSL ()
swapRoles = do
    dat <- lift get
    let o = offense dat
        d = defense dat
    lift $ put dat { offense = d, defense = o }
