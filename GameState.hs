module GameState where

import Control.Monad.State

import Types

type GameState = State GameData

getHand :: Role -> GameState [Card]
getHand role = do
    p <- case role of
        Offense -> gets offense
        Defense -> gets defense
    return $ hand p

playCard :: Card -> GameState ()
playCard card = do
    st <- get
    let t = PC card Nothing : (table st)
    put st { table = t }

-- Yeah, time to learn how to use lens
updateHand :: Role -> ([Card] -> [Card]) -> GameState ()
updateHand role tweak = do
    st <- get
    let p = case role of
                Offense -> offense st
                Defense -> defense st
        hd = tweak $ hand p
    case role of
        Offense -> put st { offense = (p { hand = hd }) }
        Defense -> put st { defense = (p { hand = hd }) }

