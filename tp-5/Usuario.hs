import SetV1 -- Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList
import QueueV1 -- Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue
import Stack -- Stack, emptyS, isEmptyS, push, top, pop, lenS

-- Library

agregarSi :: Eq a => a -> [a] -> Bool -> [a]
agregarSi x xs c = if c then (x:xs) else xs

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

-- Ejemplos

set1 = addS 3 (addS 2 (addS 1 SetV1.emptyS))

queue1 = enqueue 4 (enqueue 3 (enqueue 2 (enqueue 1 emptyQ)))
queue2 = enqueue 8 (enqueue 7 (enqueue 6 (enqueue 5 emptyQ)))

--

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s = agregarSi x (losQuePertenecen xs s) (belongs x s)

--

sinRepetidosS :: Eq a => [a] -> Set a
sinRepetidosS [] = SetV1.emptyS
sinRepetidosS (x:xs) = addS x (sinRepetidosS xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (sinRepetidosS xs)

--

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)
            deriving Show

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = SetV1.emptyS
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

-- No supe interpretar cuando se altera el orden, asi que supuse que si vos apilas y desapilas una lista el resultado tiene que quedar igual que antes de apilar

apilar :: [a] -> Stack a
apilar [] = Stack.emptyS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
desapilar s = if isEmptyS s then [] else top s : desapilar (pop s)

insertarEnPos :: Int -> a -> Stack a -> Stack a
-- PRECOND: Debe ser una posición válida en el Stack
insertarEnPos pos x s = if pos == 0 then push x s else push (top s) (insertarEnPos (pos - 1) x (pop s))

