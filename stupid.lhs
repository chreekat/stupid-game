> import System.Environment (getArgs)
> import Control.Applicative ((<$>))
> import Control.Monad ((<=<))
> import Control.Monad.State
> import Control.Monad.Free
> import Control.Monad.Trans.Free (FreeT(..))
> import qualified Control.Monad.Trans.Free as FT
> import qualified Data.Text.IO as T
> import Data.Maybe

> import Types
> import ParseInput

> main = do
>     inFile <- fmap head getArgs
>     input <- T.readFile inFile
>     mapM_ runProg (parseInput input)

> runProg :: GameData -> IO ()
> runProg = flip interp attackStage

> attackStage = do
>     x <- fromJust <$> smallestCard Offense
>     playCard Offense x
>     passStage

passStage = do
    r <- attackRank
    nOff <- handSize Offense
    nTable <- tableSize
    card <- fromJust <$> smallesteCard Defense
    if defenseHasRank r && nOff > nTable
        then do
            playCard Defense card
            swapRoles
            passStage
        else defendStage

> data GameAction nxt
>     = SmallestCard Role (Maybe Card -> nxt)
>     | PlayCard Role Card nxt
>     deriving (Functor)
>

> smallestCard :: Role -> Free GameAction (Maybe Card)
> smallestCard role = liftF $ SmallestCard role id
>
> playCard :: Role -> Card -> Free GameAction ()
> playCard role card = liftF $ PlayCard role card ()

> passStage = Pure ()

> interp :: GameData -> Free GameAction r -> IO ()
> interp start act = case act of
>     (Free (SmallestCard role f)) -> do
>         let acc = case role of Offense -> offense
>                                Defense -> defense
>         interp start (f (Just $ Card Heart R3))
>     (Free (PlayCard role card f)) -> do
>         putStrLn $ show role ++ " plays " ++ show card
>         putStrLn $ show start
>         interp start f
>     Pure _ -> putStrLn "Pured out"


> interpT :: GameData -> FreeT GameAction (State GameData) r -> IO ()
> interpT st act =
>     let (act', st') = runState (runFreeT act) st
>     in case act' of
>         (FT.Free (SmallestCard role f)) -> do
>             let acc = case role of Offense -> offense
>                                    Defense -> defense
>             interpT st' (f (Just $ Card Heart R3))
>         (FT.Free (PlayCard role card f)) -> do
>             putStrLn $ show role ++ " plays " ++ show card
>             putStrLn $ show st'
>             interpT st' f
>         FT.Pure _ -> putStrLn "Pured out"
