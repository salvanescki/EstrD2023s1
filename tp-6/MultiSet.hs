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
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
multiSetToList :: Ord a => MultiSet a -> [(a, Int)]

--

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

fromJust :: Maybe a -> a
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

mergeMapsK:: Ord k => [k] -> Map k v -> Map k v -> Map k v
mergeMapsK [] _ m2 = m2
mergeMapsK (k:ks) m1 m2 = assocM k (fromJust(lookupM k m1)) (mergeMapsK ks m1 m2)

mergeMaps:: Ord k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = mergeMapsK (keys m1) m1 m2

mapDe :: MultiSet a -> Map a Int
mapDe (MS m) = m 

ocurrencesMap :: Ord a => a -> Map a Int -> Int
ocurrencesMap x m = fromJust(lookupM x m)

intersectionMap :: Ord a => [a] -> [a] -> Map a Int -> Map a Int -> Map a Int
intersectionMap [] _ _ _ = emptyM
intersectionMap (x:xs) ys m1 m2 = if pertenece x ys
                                    then assocM x ((ocurrencesMap x m1) + (ocurrencesMap x m2)) (intersectionMap xs ys m1 m2)
                                    else intersectionMap xs ys m1 m2

mapToListK :: Ord k => [k] -> Map k v -> [(k, v)]
mapToListK [] _ = []
mapToListK (k:ks) mp = (k, ocurrencesMap k mp) : mapToListK ks mp

mapToList :: Ord k => Map k v -> [(k, v)]
mapToList mp = mapToListK (keys mp) mp

--

emptyMS = MS emptyM
addMS x (MS m) = if pertenece x (keys m)
                    then MS (assocM x ((ocurrencesMap x m) + 1) m)
                    else MS (assocM x 1 m)
ocurrencesMS x (MS m) = ocurrencesMap x m
unionMS (MS m1) (MS m2) = MS (mergeMaps m1 m2)
intersectionMS (MS m1) (MS m2) = MS (intersectionMap (keys m1) (keys m2) m1 m2)
multiSetToList (MS m) = mapToList m