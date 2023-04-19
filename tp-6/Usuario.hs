import PriorityQueue -- PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ

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

