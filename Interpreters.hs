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
            putStrLn $ "\n\nNew round"
            putStrLn $ "Offense attacks with " ++ show card
            putStrLn $ show st'
            interpT st' f
        (FT.Free (PassWith card f)) -> do
            putStr $ "Defense passes with " ++ show card
            putStrLn $ "; roles switch\n" ++ show st'
            interpT st' f
        (FT.Free (Defend pc c f)) -> do
            putStrLn $ "Covering " ++ (show . card) pc ++ " with " ++ show c
            putStrLn $ show st'
            interpT st' f
        (FT.Free (ReinforceWith cards f)) -> do
            putStrLn $ "Offense reinforces with " ++ show cards
            putStrLn $ show st'
            interpT st' f
        (FT.Free (WinTurn role f)) -> do
            putStrLn $ show role ++ " wins that turn."
            putStrLn $ show st'
            interpT st' f
        (FT.Free (Result n f)) -> do
            putStrLn $ "\n\n" ++ show n
            interpT st' f
        FT.Pure _ -> return ()
