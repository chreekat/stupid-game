module Interpreters where

import Control.Monad.State
import qualified Control.Monad.Trans.Free as FT

import Types
import DSL

debugInterp :: GameData -> GameDSL r -> IO ()
debugInterp st act =
    let (act', st') = runState (FT.runFreeT act) st
    in case act' of
        (FT.Free (AttackWith card f)) -> do
            putStrLn $ "\n\nNew round"
            putStrLn $ "Offense attacks with " ++ show card
            putStrLn $ show st'
            debugInterp st' f
        (FT.Free (PassWith card f)) -> do
            putStr $ "Defense passes with " ++ show card
            putStrLn $ "; roles switch\n" ++ show st'
            debugInterp st' f
        (FT.Free (Defend pc c f)) -> do
            putStrLn $ "Covering " ++ (show . card) pc ++ " with " ++ show c
            putStrLn $ show st'
            debugInterp st' f
        (FT.Free (ReinforceWith cards f)) -> do
            putStrLn $ "Offense reinforces with " ++ show cards
            putStrLn $ show st'
            debugInterp st' f
        (FT.Free (WinTurn role f)) -> do
            putStrLn $ show role ++ " wins that turn."
            putStrLn $ show st'
            debugInterp st' f
        (FT.Free (Result n f)) -> do
            putStrLn $ "\n\n" ++ show n
            debugInterp st' f
        FT.Pure _ -> return ()

quietInterp :: GameData -> GameDSL r -> IO ()
quietInterp st act =
    let (act', st') = runState (FT.runFreeT act) st
    in case act' of
        (FT.Free (AttackWith card f)) -> do
            quietInterp st' f
        (FT.Free (PassWith card f)) -> do
            quietInterp st' f
        (FT.Free (Defend pc c f)) -> do
            quietInterp st' f
        (FT.Free (ReinforceWith cards f)) -> do
            quietInterp st' f
        (FT.Free (WinTurn role f)) -> do
            quietInterp st' f
        (FT.Free (Result n f)) -> do
            putStr $ show n
            quietInterp st' f
        FT.Pure _ -> return ()
