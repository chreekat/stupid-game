{-# LANGUAGE DeriveFunctor #-}

module DSL where

import Data.List (delete)
import Control.Error
import Control.Exception
import Control.Monad.State
import Control.Monad.Trans.Free

import Types
import GameState as GS

data GameAction nxt
    = SmallestCard Role (Maybe Card) (Maybe Card -> nxt)
    | PlayCard Role Card nxt
    deriving (Functor)

type GameDSL = FreeT GameAction GameState

smallestCard :: Role -> GameDSL (Maybe Card)
smallestCard role = do
    hand <- lift $ getHand role
    let c = headMay hand
    liftF $ SmallestCard role c id

playCard :: Role -> Card -> GameDSL ()
playCard role card = do
    hand <- lift $ getHand role
    lift $ updateHand role (delete card)
    lift $ GS.playCard card
    liftF $ PlayCard role card ()
