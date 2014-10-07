import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>))
import Control.Error
import Control.Monad ((<=<))
import Control.Monad.Free
import Control.Monad.Trans.Free (FreeT(..))
import Control.Monad.State
import qualified Control.Monad.Trans.Free as FT
import qualified Data.Text.IO as T
import Data.Maybe

import Types
import ParseInput
import DSL
import Interpreters

main = do
    inFile <- fmap head getArgs
    input <- T.readFile inFile
    mapM_ runProg (parseInput input)

runProg :: GameData -> IO ()
runProg = flip interpT attackStage

attackStage = do
    hand <- getHand Offense
    c@(Card _ rank) <- fromJust <$> smallestCard hand
    playCard Offense c
    passStage rank

passStage rank = do
    nOff <- handSize Offense
    nTable <- tableSize
    card <- minimumMay <$> (cardsRanked rank Defense)
    case (compare nOff nTable, card) of
        -- offense has enough cards && defense has one to play
        (GT, Just card') -> do
            playCard Defense card'
            swapRoles
            passStage rank
        -- else...
        _ -> defendStage

defendStage = do
    hand <- getHand Defense
    mTarget <- minimumMay <$> uncoveredCards
    mVolunteer <- runMaybeT $ do
        target <- hoistMaybe mTarget
        headMay <$> volunteers target

    case (mTarget, mVolunteer) of
        (Just tar, Just vol) -> defend tar vol
        -- No card OR the card could not be defended against
        _                   -> reinforceStage

reinforceStage = do
    nUnc <- length <$> uncoveredCards
    nDef <- length <$> getHand Defense
    reinforcements <- (take (nDef - nUnc)) <$> possibleReinforcements

    case reinforcements of
        [] -> verdictStage nUnc
        _  -> do
            reinforceWith reinforcements
            defendStage

verdictStage numUncovered = do
    case numUncovered of
        0 -> winTurn Defense
        _ -> winTurn Offense

    (oHand, dHand) <- (,)
        <$> getHand Offense
        <*> getHand Defense

    case (oHand, dHand) of
        ([], []) -> tieGame
        (_ , []) -> winner Defense
        ([],  _) -> winner Offense
        _        -> attackStage
