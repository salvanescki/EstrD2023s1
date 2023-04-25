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

ejMap = assocM "4" 4
        $ assocM "3" 3
        $ assocM "2" 2
        $ assocM "1" 1
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

