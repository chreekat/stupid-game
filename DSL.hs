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
    | WinTurn Role nxt
    | Result Int nxt
    deriving (Functor)

type GameDSL = FreeT GameAction GameState

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

passWith card = do
    playCard Defense card
    swapRoles
    liftF $ PassWith card id

swapRoles :: GameDSL ()
swapRoles = lift $ GS.swapRoles

defend :: PlayedCard -> Card -> GameDSL ()
defend pc c = do
    table <- getTable
    hand <- getHand Defense
    assert (pc `elem` table)
    assert (c `elem` hand)
    lift $ GS.updateHand Defense (delete c)
    lift $ GS.coverCard pc c
    liftF $ Defend pc c ()

playedCards :: GameDSL [Card]
playedCards = do
    t <- getTable
    return $ concatMap bothCards t

  where

    bothCards (PC ca co) = case co of
        Just co' -> [ca, co']
        _        -> [ca]

reinforceWith :: [Card] -> GameDSL ()
reinforceWith cs = do
    mapM_ (playCard Offense) cs
    liftF $ ReinforceWith cs ()

winTurn role = do
    case role of
        Offense -> do
            allCards <- playedCards
            lift $ GS.updateHand Defense (++ allCards)
        Defense -> swapRoles
    lift $ GS.clearTable
    liftF $ WinTurn role ()

tieGame :: GameDSL ()
tieGame = liftF $ Result 0 ()

winner :: Role -> GameDSL ()
winner role = do
    n <- playerNum role
    liftF $ Result n ()

playerNum :: Role -> GameDSL Int
playerNum r = lift $ GS.playerNum r
