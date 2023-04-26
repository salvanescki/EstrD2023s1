module MapV1
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
  where

data Map k v = M [(k,v)]
  {- INV.REP.: en M kvs
        * No hay keys repetidas en kvs -}

emptyM  :: Map k v                                         -- O(1)
assocM  :: Ord k => k -> v -> Map k v -> Map k v           -- O(n)
lookupM :: Ord k => k -> Map k v -> Maybe v                -- O(n)
deleteM :: Ord k => k -> Map k v -> Map k v                -- O(n)
keys    :: Ord k => Map k v -> [k]                         -- O(n)

emptyM              = M []
assocM  k v (M kvs) = M (asociar k v kvs)
lookupM k   (M kvs) = buscar k kvs
deleteM k   (M kvs) = M (borrar k kvs)
keys        (M kvs) = claves kvs

-- O(n). Siendo n la cantidad de pares. Por cada par kv, realiza operaciones de costo O(1) (Cons Just, ==)
buscar :: Eq k => k -> [(k,v)] -> Maybe v
buscar k []            = Nothing
buscar k ((k',v'):kvs) = if k==k' then Just v' else buscar k kvs

-- O(n). Siendo n la cantidad de pares. Por cada par kv, realiza un cons O(1).
claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs

-- O(n). Siendo n la cantidad de pares. Por cada par kv, realiza operaciones de costo O(1) (cons, ==)
asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = [ (k,v) ]
asociar k v ((k',v'):kvs) = if k==k' then (k',v):kvs else (k', v') : asociar k v kvs

-- O(n). Siendo n la cantidad de pares. Por cada par kv, realiza operaciones de costo O(1) (cons, ==)
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar k []            = []
borrar k ((k',v'):kvs) = if k==k' then kvs else (k',v') : borrar k kvs