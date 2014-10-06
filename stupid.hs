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
    x <- fromJust <$> smallestCard Offense
    playCard Offense x
    passStage

passStage = do
    nOff <- handSize Offense
    nTable <- tableSize
    r <- attackRank
    card <- minimumMay <$> (cardsRanked r Defense)
    case (compare nOff nTable, card) of
        -- offense has enough cards && defense has one to play
        (GT, Just card'@(Card s r')) -> do
            playCard Defense card'
            swapRoles
            passStage
        -- else...
        _ -> defendStage

defendStage = do
    mTarget <- minimumMay <$> uncoveredCards
    success <- coverCard' mTarget
    case (mTarget, success) of
        -- There was a card, it was defended
        (Just _, Just True) -> defendStage
        -- No card OR the card could not be defended against
        _                   -> reinforceStage

  where

    coverCard' :: Maybe Card -> GameDSL (Maybe Bool)
    coverCard' mTarget = case mTarget of
        Just target -> Just <$> coverCard target
        Nothing     -> return $ Nothing

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
