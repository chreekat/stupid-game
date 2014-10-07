module CardFuncs where

import Data.List

import Types

compareCard t c1@(Card s1 r1) c2@(Card s2 r2) =
    let t1 = s1 == t
        t2 = s2 == t
    in
        case (t1, t2) of
            (True, True) -> compare r1 r2
            (True, False) -> GT
            (False, True) -> LT
            (False, False) -> compare c1 c2

comparePlayedCard t c1 c2 = compareCard t (card c1) (card c2)

minCard t = minimumByMay (compareCard t)

minPlayedCard t = minimumByMay (comparePlayedCard t)

minimumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMay f ls = case ls of
    [] -> Nothing
    _  -> Just $ minimumBy f ls
