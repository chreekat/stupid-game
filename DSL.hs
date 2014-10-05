{-# LANGUAGE DeriveFunctor #-}

module DSL where

import Data.List (delete)
import Control.Error
import Control.Exception
import Control.Monad.State
import Control.Monad.Trans.Free

import Types

data Role = Offense | Defense deriving (Show)

data GameAction nxt
    = SmallestCard Role (Maybe Card) (Maybe Card -> nxt)
    | PlayCard Role Card nxt
    deriving (Functor)

type GameDSL = FreeT GameAction (State GameData)

smallestCard :: Role -> GameDSL (Maybe Card)
smallestCard role = do
    hand <- lift $ getHand role
    let c = headMay hand
    liftF $ SmallestCard role c id

getHand :: Role -> State GameData [Card]
getHand role = do
    p <- case role of
        Offense -> gets offense
        Defense -> gets defense
    return $ hand p

-- Yeah, time to learn how to use lens
updateHand :: Role -> ([Card] -> [Card]) -> State GameData ()
updateHand role tweak = do
    st <- get
    let p = case role of
                Offense -> offense st
                Defense -> defense st
        hd = tweak $ hand p
    case role of
        Offense -> put st { offense = (p { hand = hd }) }
        Defense -> put st { defense = (p { hand = hd }) }

playCard' :: Card -> State GameData ()
playCard' card = do
    st <- get
    let t = PC card Nothing : (table st)
    put st { table = t }

playCard :: Role -> Card -> GameDSL ()
playCard role card = do
    hand <- lift $ getHand role
    lift $ updateHand role (delete card)
    lift $ playCard' card
    liftF $ PlayCard role card ()
