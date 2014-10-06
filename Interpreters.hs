module Interpreters where

interpT = undefined

{-
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
        (FT.Free (CoverCard covered success f)) -> do
            if success
                then putStrLn $ "Covered " ++ show covered
                else putStrLn $ "Could not cover" ++ show covered
            interpT st' (f success)
        FT.Pure _ -> return ()
        -}
