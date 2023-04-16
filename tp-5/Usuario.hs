import SetV1 -- Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList
import QueueV1 -- Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue

-- Library

agregarSi :: Eq a => a -> [a] -> Bool -> [a]
agregarSi x xs c = if c then (x:xs) else xs

-- Ejemplos

set1 = addS 3 (addS 2 (addS 1 emptyS))

queue1 = enqueue 4 (enqueue 3 (enqueue 2 (enqueue 1 emptyQ)))
queue2 = enqueue 8 (enqueue 7 (enqueue 6 (enqueue 5 emptyQ)))

--

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s = agregarSi x (losQuePertenecen xs s) (belongs x s)

--

sinRepetidosS :: Eq a => [a] -> Set a
sinRepetidosS [] = emptyS
sinRepetidosS (x:xs) = addS x (sinRepetidosS xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (sinRepetidosS xs)

--

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)
            deriving Show

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))

--

lengthQ :: Queue a -> Int
lengthQ q = if (isEmptyQ q) then 0 else 1 + lengthQ (dequeue q)

--

queueToList :: Queue a -> [a]
queueToList q = if (isEmptyQ q) then [] else firstQ q : queueToList (dequeue q)

--

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if (isEmptyQ q2) then q1 else enqueue (firstQ q2) (unionQ q1 (dequeue q2))