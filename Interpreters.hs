module Interpreters where

import Control.Monad.State
import qualified Control.Monad.Trans.Free as FT

import Types
import DSL

interpT :: GameData -> GameDSL r -> IO ()
interpT st act =
    let (act', st') = runState (FT.runFreeT act) st
    in case act' of
        (FT.Free (AttackWith card f)) -> do
            putStrLn $ "Offense attacks with " ++ show card
            putStrLn $ show st'
            interpT st' f
        (FT.Free (PassWith card f)) -> do
            putStr $ "Defense passes with " ++ show card
            putStrLn $ "; roles switch\n" ++ show st'
            interpT st' f
        (FT.Free (ReinforceWith cards f)) -> do
            putStrLn $ "Offense reinforces with " ++ show cards
            putStrLn $ show st'
            interpT st' f
        FT.Pure _ -> return ()
