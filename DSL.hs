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
    | Defend PlayedCard Card nxt
    | ReinforceWith [Card] nxt
    deriving (Functor)

type GameDSL = FreeT GameAction GameState

tieGame = undefined
winner = undefined
winTurn = undefined

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

getTable :: GameDSL [PlayedCard]
getTable = lift GS.getTable

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

defend :: PlayedCard -> Card -> GameDSL ()
defend pc c = do
    table <- getTable
    assert (pc `elem` table)
    lift $ GS.coverCard pc c
    liftF $ Defend pc c ()

playedCards :: GameDSL [Card]
playedCards = do
    t <- lift $ gets table
    return $ concatMap bothCards t

  where

    bothCards (PC ca co) = case co of
        Just co' -> [ca, co']
        _        -> [ca]

reinforceWith :: [Card] -> GameDSL ()
reinforceWith cs = do
    mapM_ (playCard Offense) cs
    liftF $ ReinforceWith cs ()
