import System.Environment (getArgs)
import Control.Applicative ((<$>))
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
        (GT, Just card'@(Card s r')) -> do
            playCard Defense card'
            swapRoles
            passStage
        _ -> defendStage

defendStage = return $ Pure ()

interpT :: GameData -> GameDSL r -> IO ()
interpT st act =
    let (act', st') = runState (runFreeT act) st
    in case act' of
        (FT.Free (SmallestCard role card f)) -> do
            putStrLn $ show role ++ "'s smallest card is " ++ show card
            interpT st' (f card)
        (FT.Free (PlayCard role card f)) -> do
            putStrLn $ show role ++ " plays " ++ show card
            putStrLn $ show st'
            interpT st' f
        (FT.Free (AttackRank card f)) -> do
            putStrLn $ "Attack rank is " ++ show card
            interpT st' (f card)
        FT.Pure _ -> return ()
