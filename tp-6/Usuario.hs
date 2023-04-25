import PriorityQueue -- PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ
import Map -- Map, emptyM, assocM, lookupM, deleteM, keys
--

-- O(n^2) ya que aplica findMinPQ y deleteMinPQ [ambas O(n)] para cada elemento en la PQ (misma cantidad que la lista)
priorityQueueToList :: Ord a => PriorityQueue a -> [a]
priorityQueueToList pq = if isEmptyPQ pq then [] else findMinPQ pq : priorityQueueToList (deleteMinPQ pq)

-- O(n) siendo n la cantidad de elementos de la lista
listToPriorityQueue :: Ord a => [a] -> PriorityQueue a
listToPriorityQueue [] = emptyPQ
listToPriorityQueue (x:xs) = insertPQ x (listToPriorityQueue xs)

-- O(n^2)
heapSort :: Ord a => [a] -> [a]
heapSort xs = priorityQueueToList (listToPriorityQueue xs)

--

ejMap :: Map String Int
ejMap = assocM "4" 4
        $ assocM "3" 3
        $ assocM "2" 2
        $ assocM "a" 1
        $ emptyM

ejMap2 :: Map String Int
ejMap2 = assocM "d" 8
        $ assocM "c" 7
        $ assocM "b" 6
        $ assocM "a" 5
        $ emptyM

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

fromJust :: Maybe a -> a
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

--

valuesK :: Ord k => [k] -> Map k v -> [Maybe v]
valuesK [] _ = []
valuesK (k:ks) mp = lookupM k mp : valuesK ks mp

valuesM :: Ord k => Map k v -> [Maybe v]
valuesM mp = valuesK (keys mp) mp

--

todasAsociadas :: Ord k => [k] -> Map k v -> Bool
todasAsociadas [] _ = True
todasAsociadas (k:ks) mp = pertenece k (keys mp) && todasAsociadas ks mp

--

listToMap :: Ord k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

--

mapToListK :: Ord k => [k] -> Map k v -> [(k, v)]
mapToListK [] _ = []
mapToListK (k:ks) mp = (k, fromJust(lookupM k mp)) : mapToListK ks mp

mapToList :: Ord k => Map k v -> [(k, v)]
mapToList mp = mapToListK (keys mp) mp

--


ejLista = [
            ("a", 1),
            ("a", 2),
            ("a", 3),
            ("b", 4),
            ("b", 5),
            ("c", 6)
          ]

{-

{ "a":[1,2,3], "b":[4,5], "c":[6] }

-}

agruparEq :: Ord k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,v): kvs) = if pertenece k (keys (agruparEq kvs))
                            then assocM k (v : fromJust(lookupM k (agruparEq kvs))) (agruparEq kvs)
                            else assocM k [v] (agruparEq kvs)

--

incrementar :: Ord k => [k] -> Map k Int -> Map k Int
-- PRECOND: Las keys de la lista deben pertenecer al map
incrementar [] _ = emptyM
incrementar (k:ks) mp = assocM k (1 + fromJust(lookupM k mp)) (incrementar ks mp)

--

mergeMapsK:: Ord k => [k] -> Map k v -> Map k v -> Map k v
mergeMapsK [] _ m2 = m2
mergeMapsK (k:ks) m1 m2 = assocM k (fromJust(lookupM k m1)) (mergeMapsK ks m1 m2)

mergeMaps:: Ord k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = mergeMapsK (keys m1) m1 m2

--
