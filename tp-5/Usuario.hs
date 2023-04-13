import Set -- Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList

-- Library

agregarSi :: Eq a => a -> [a] -> Bool -> [a]
agregarSi x xs c = if c then (x:xs) else xs

-- Ejemplo Set

set1 = addS 3 (addS 2 (addS 1 emptyS))

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