module MapV2
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
  where

data Map k v = M [(k,v)]

emptyM  :: Map k v                                         -- O(1)
assocM  :: Ord k => k -> v -> Map k v -> Map k v           -- O(1)
lookupM :: Ord k => k -> Map k v -> Maybe v                -- O(n)
deleteM :: Ord k => k -> Map k v -> Map k v                -- O(n)
keys    :: Ord k => Map k v -> [k]                         -- O(n^2)

emptyM              = M []
assocM  k v (M kvs) = M ((k,v):kvs)
lookupM k   (M kvs) = buscar k kvs
deleteM k   (M kvs) = M (borrar k kvs)
keys        (M kvs) = claves kvs

-- Aux
pertenece :: Eq a => a -> [a] -> Bool -- O(n)
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- O(n). Siendo n la cantidad de pares de la lista. Ya que aplica operaciones de costo constante por cada elemento de la lista (Cons Just, ==) 
buscar :: Eq k => k -> [(k,v)] -> Maybe v
buscar k []            = Nothing
buscar k ((k',v'):kvs) = if k==k' then Just v' else buscar k kvs

-- O(n^2). Siendo n la cantidad de pares de la lista. Ya que aplica pertenece O(n) por cada par de la lista
claves :: Eq k => [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = if pertenece k (claves kvs)
                        then claves kvs
                        else k : claves kvs

-- O(n). Siendo n la cantidad de pares de la lista. Ya que aplica operaciones de costo constante por cada elemento de la lista (cons, ==)
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar k []            = []
borrar k ((k',v'):kvs) = if k==k' then kvs else (k',v') : borrar k kvs