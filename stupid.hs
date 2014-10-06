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
        (GT, Just card'@(Card s r')) -> do
            playCard Defense card'
            swapRoles
            passStage
        _ -> defendStage

defendStage = return $ Pure ()
