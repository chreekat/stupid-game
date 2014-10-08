import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>))
import Control.Error
import qualified Data.Text.IO as T
import Data.Maybe (fromJust)

import Types
import ParseInput
import DSL
import CardFuncs
import Interpreters

main = do
    inFile <- fmap head getArgs
    input <- T.readFile inFile
    mapM_ runProg (parseInput input)
    putStr "\r\n"

runProg :: GameData -> IO ()
runProg = flip quietInterp attackStage

attackStage = do
    t <- getTrump
    c@(Card _ rank) <- fromJust . (minCard t) <$> getHand Offense
    attackWith c
    passStage rank

passStage rank = do
    t <- getTrump
    nOff <- length <$> getHand Offense
    nTable <- length <$> getTable
    card <- (minCard t . cardsRanked rank) <$> getHand Defense
    case (compare nOff nTable, card) of
        -- offense has enough cards && defense has one to play
        (GT, Just card') -> do
            passWith card'
            passStage rank
        -- else...
        _ -> defendStage

  where

    cardsRanked r = filter ((r ==) . cRank)

defendStage = do
    t <- getTrump
    hand <- getHand Defense
    mTarget <- minPlayedCard t <$> uncoveredCards
    mVolunteer <- runMaybeT $ do
        target <- hoistMaybe mTarget
        volunteer (card target)

    case (mTarget, mVolunteer) of
        (Just tar, Just vol) -> do
            defend tar vol
            defendStage
        _  -> reinforceStage

uncoveredCards :: GameDSL [PlayedCard]
uncoveredCards = filter ((== Nothing) . cover) <$> getTable

volunteer :: Card -> MaybeT GameDSL Card
volunteer card@(Card suit _) = MaybeT $ do
    t <- getTrump
    hand <- getHand Defense
    let sameSuit = filter (> card) $ cardsSuited suit hand
        trumps   = if suit /= t
                   then cardsSuited t hand
                   else [] -- trumps already exist in sameSuit
        volunteers = sameSuit ++ trumps
    return $ minCard t volunteers

  where

    cardsSuited s = filter ((s ==) . cSuit)

reinforceStage = do
    nUnc <- length <$> uncoveredCards
    nDef <- length <$> getHand Defense
    reinforcements <- (take (nDef - nUnc)) <$> possibleReinforcements

    case reinforcements of
        [] -> verdictStage nUnc
        _  -> do
            reinforceWith reinforcements
            defendStage

  where

    possibleReinforcements = do
        ranks <- map cRank <$> playedCards
        filter ((`elem` ranks) . cRank) <$> getHand Offense

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
