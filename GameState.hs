module GameState where

import Control.Monad.State

import Types

type GameState = State GameData

getTrump :: GameState Suit
getTrump = gets trump

getTable :: GameState [PlayedCard]
getTable = gets table

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

coverCard :: PlayedCard -> Card -> GameState ()
coverCard pc c = do
    st <- get
    table <- gets table
    let table' = map cover' table
    put st { table = table }

  where

    cover' x = if x == pc
        then x { cover = Just c }
        else x


