module PriorityQueue
  (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
 where

data PriorityQueue a = PQ [a]

emptyPQ     :: PriorityQueue a                                      -- O(1)
isEmptyPQ   :: PriorityQueue a -> Bool                              -- O(1)
insertPQ    :: Ord a => a -> PriorityQueue a -> PriorityQueue a     -- O(1)
findMinPQ   :: Ord a => PriorityQueue a -> a                        -- O(n)
  -- PRECOND: La PQ no debe estar vacía. Parcial
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a          -- O(n)
  -- PRECOND: La PQ no debe estar vacía. Parcial

emptyPQ             = PQ []
isEmptyPQ (PQ xs)   = null xs
insertPQ x (PQ xs)  = PQ (x:xs)
findMinPQ (PQ xs)   = minimum xs
deleteMinPQ (PQ xs) = PQ (borrarMin xs)

-- O(n)
borrarMin :: Ord a => [a] -> [a]
  -- PRECOND: la lista no es vacía
borrarMin xs = borrar (minimum xs) xs

-- O(n)
borrar :: Eq a => a -> [a] -> [a]
borrar x []     = []
borrar x (y:ys) = if x==y then ys else y : borrar x ys