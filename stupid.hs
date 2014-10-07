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
import CardFuncs
import Interpreters

main = do
    inFile <- fmap head getArgs
    input <- T.readFile inFile
    mapM_ runProg (parseInput input)

runProg :: GameData -> IO ()
runProg = flip interpT attackStage

attackStage = do
    t <- getTrump
    c@(Card _ rank) <- fromJust . (minCard t) <$> getHand Offense
    attackWith c
    passStage rank

passStage rank = do
    t <- getTrump
    nOff <- length <$> getHand Offense
    nTable <- tableSize
    card <- minCard t <$> (cardsRanked rank Defense)
    case (compare nOff nTable, card) of
        -- offense has enough cards && defense has one to play
        (GT, Just card') -> do
            passWith card'
            passStage rank
        -- else...
        _ -> defendStage

defendStage = do
    t <- getTrump
    hand <- getHand Defense
    mTarget <- minPlayedCard t <$> uncoveredCards
    mVolunteer <- runMaybeT $ do
        target <- hoistMaybe mTarget
        volunteer (card target)

    case (mTarget, mVolunteer) of
        (Just tar, Just vol) -> defend tar vol
        -- No card OR the card could not be defended against
        _                   -> reinforceStage

volunteer :: Card -> MaybeT GameDSL Card
volunteer card@(Card _ rank) = MaybeT $ do
    t <- getTrump
    draftees <- filter (> card) <$> cardsRanked rank Defense
    case draftees of
        [] -> headMay <$> cardsRanked t Defense
        _  -> return $ Just $ head draftees

reinforceStage = do
    nUnc <- length <$> uncoveredCards
    nDef <- length <$> getHand Defense
    reinforcements <- (take (nDef - nUnc)) <$> possibleReinforcements

    case reinforcements of
        [] -> verdictStage nUnc
        _  -> do
            reinforceWith reinforcements
            defendStage

possibleReinforcements = do
    suits <- (map (\(Card s _) -> s)) <$> playedCards
    filter ((`elem` suits) . \(Card s _) -> s) <$> getHand Offense

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
