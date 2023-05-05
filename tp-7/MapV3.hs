module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
  where

data Map k v = M [k] [v]
  {- INV.REP.: en M ks vs
        * ks y vs tienen la misma longitud
        * No hay keys repetidas en ks
  -}

emptyM  :: Map k v                                         -- O(1)
assocM  :: Ord k => k -> v -> Map k v -> Map k v           -- O(n)
lookupM :: Ord k => k -> Map k v -> Maybe v                -- O(n)
deleteM :: Ord k => k -> Map k v -> Map k v                -- O(n)
keys    :: Ord k => Map k v -> [k]                         -- O(1)

emptyM             = M [] []
assocM k v (M ks vs) = M (fst (asociar k v ks vs)) (snd (asociar k v ks vs))
lookupM k  (M ks vs) = buscar k ks vs
deleteM k  (M ks vs) = M (fst (borrar k ks vs)) (snd (borrar k ks vs))
keys       (M ks vs) = ks

-- Aux

consAParDeListas :: a -> b -> ([a],[b]) -> ([a],[b]) -- O(1)
consAParDeListas x y (xs,ys) = (x:xs, y:ys)

-- O(n). Siendo n la cantidad de keys/values en las listas. Ya que aplica operaciones de costo constante por cada elemento de las listas ks y vs.
asociar :: Eq k => k -> v -> [k] -> [v] -> ([k],[v])
asociar k' v' [] [] = ([k'],[v'])
asociar k' v' (k:ks) (v:vs) = if k'==k
                                then consAParDeListas k v' (ks,vs)
                                else consAParDeListas k v (asociar k' v' ks vs)

-- O(n). Siendo n la cantidad de claves(repetidas)/valores del Map. Se aplica por cada clave, operaciones de costo constante (Cons Just, ==)
buscar :: Eq k => k -> [k] -> [v] -> Maybe v
buscar _ [] _ = Nothing
buscar k' (k:ks) (v:vs) = if k'==k
                          then Just v
                          else buscar k' ks vs

-- O(n). Siendo n la cantidad de elementos de alguna de las listas. Aplica operaciones constantes por cada elemento.
borrar :: Ord k => k -> [k] -> [v] -> ([k],[v])
borrar _ [] _ = ([],[])
borrar k' (k:ks) (v:vs) = if k'==k
                            then (ks,vs)
                            else consAParDeListas k v (borrar k' ks vs)