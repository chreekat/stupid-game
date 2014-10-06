{-# LANGUAGE DeriveFunctor #-}

module DSL where

import Data.List (delete, partition, find)
import Control.Applicative ((<$>))
import Control.Error
import qualified Control.Exception as Ex (assert)
import Control.Monad.State
import Control.Monad.Free
import Control.Monad.Trans.Free (FreeT(..), liftF)

import Types
import GameState as GS

data GameAction nxt
    = SmallestCard Role (Maybe Card) (Maybe Card -> nxt)
    | PlayCard Role Card nxt
    | AttackRank Rank (Rank -> nxt)
    deriving (Functor)

type GameDSL = FreeT GameAction GameState

-- | assertions for the DSL programmer
assert test = return $ Ex.assert test $ Pure ()

smallestCard :: Role -> GameDSL (Maybe Card)
smallestCard role = do
    hand <- lift $ getHand role
    t <- lift $ gets trump
    let c = minCard hand t
    liftF $ SmallestCard role c id

  where

    minCard cards t =
        let (trumpCards, rest) = partition (\(Card s _) -> s == t) cards
        in case rest of
            [] -> minimumMay trumpCards
            _  -> minimumMay rest

playCard :: Role -> Card -> GameDSL ()
playCard role card = do
    hand <- lift $ getHand role
    assert (card `elem` hand)
    lift $ updateHand role (delete card)
    lift $ GS.playCard card
    liftF $ PlayCard role card ()

attackRank :: GameDSL Rank
attackRank = do
    t <- lift $ gets table
    assert $ (0 < length t)
    let c = (\(Card _ r) -> r) . card . head $ t
    liftF $ AttackRank c id

swapRoles :: GameDSL ()
swapRoles = do
    dat <- lift get
    let o = offense dat
        d = defense dat
    lift $ put dat { offense = d, defense = o }

cardsRanked :: Rank -> Role -> GameDSL [Card]
cardsRanked r role = do
    hand <- lift $ getHand role
    return $ filter (\(Card s r') -> (r' == r)) hand

handSize :: Role -> GameDSL Int
handSize role = length <$> lift (getHand role)

tableSize :: GameDSL Int
tableSize = length <$> lift (gets table)
