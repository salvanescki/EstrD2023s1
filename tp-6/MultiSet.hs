module MultiSet
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import MapV3 -- Map, emptyM, assocM, lookupM, deleteM, keys

data MultiSet a = MS (Map a Int)
{- INV.REP:
-}

emptyMS :: MultiSet a
addMS :: Ord a => a -> MultiSet a -> MultiSet a
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a          -- ARREGLAR
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
multiSetToList :: Ord a => MultiSet a -> [(a, Int)]

--
ejMS = addMS "a"
        $ addMS "a"
        $ addMS "a"
        $ addMS "a"
        $ addMS "b"
        $ addMS "b"
        $ addMS "b"
        $ addMS "c"
        $ addMS "c"
        $ emptyMS

ejMS2 = addMS "a"
        $ addMS "a"
        $ addMS "d"
        $ addMS "d"
        $ addMS "e"
        $ addMS "e"
        $ addMS "e"
        $ addMS "f"
        $ addMS "f"
        $ emptyMS

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

fromJust :: Maybe a -> a
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

unionMap :: Ord a => [a] -> [a] -> Map a Int -> Map a Int -> Map a Int
unionMap [] _ _ m2 = m2
unionMap (x:xs) ys m1 m2 = if pertenece x ys
                                    then assocM x ((ocurrencesMap x m1) + (ocurrencesMap x m2)) (unionMap xs ys m1 m2)
                                    else assocM x (ocurrencesMap x m1) (unionMap xs ys m1 m2)

mapDe :: MultiSet a -> Map a Int
mapDe (MS m) = m 

ocurrencesMap :: Ord a => a -> Map a Int -> Int
ocurrencesMap x m = fromJust(lookupM x m)

intersectionMap :: Ord a => [a] -> [a] -> Map a Int -> Map a Int -> Map a Int
intersectionMap [] _ _ _ = emptyM
intersectionMap (x:xs) ys m1 m2 = if pertenece x ys
                                    then assocM x ((ocurrencesMap x m1) + (ocurrencesMap x m2)) (intersectionMap xs ys m1 m2)
                                    else intersectionMap xs ys m1 m2

multiSetToListK :: Ord a => [a] -> Map a Int -> [(a, Int)]
multiSetToListK [] _ = []
multiSetToListK (k:ks) mp = (k, (fromJust(lookupM k mp))) : multiSetToListK ks mp

--

emptyMS = MS emptyM
addMS x (MS m) = if pertenece x (keys m)
                    then MS (assocM x ((ocurrencesMap x m) + 1) m)
                    else MS (assocM x 1 m)
ocurrencesMS x (MS m) = ocurrencesMap x m
unionMS (MS m1) (MS m2) = MS (unionMap (keys m1) (keys m2) m1 m2)
intersectionMS (MS m1) (MS m2) = MS (intersectionMap (keys m1) (keys m2) m1 m2)
multiSetToList (MS m) = multiSetToListK (keys m) m