module SetV2 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a]

emptyS :: Set a                                             -- O(1)
addS :: Eq a => a -> Set a -> Set a                         -- O(1)
belongs :: Eq a => a -> Set a -> Bool                       -- O(n) con n los elementos de la lista (puede ser mayor al n del Set)
sizeS :: Eq a => Set a -> Int                               -- O(n^2) con n los elementos de la lista (puede ser mayor al n del Set)
removeS :: Eq a => a -> Set a -> Set a                      -- O(n)
unionS :: Eq a => Set a -> Set a -> Set a                   -- O(n) con n los elementos de algunas de las listas o su suma (puede ser mayor al n de sus Sets)
setToList :: Eq a => Set a -> [a]                           -- O(n^2) con n los elementos de la lista (puede ser mayor al n del Set)

pertenece :: Eq a => a -> [a] -> Bool   -- O(n)
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

sinRepetir :: Eq a => [a] -> [a]    -- O(n^2) en el peor caso
sinRepetir [] = []
sinRepetir (x:xs) = if pertenece x xs
                        then xs
                        else x:xs

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

emptyS = S []
addS x (S xs) = S (x:xs)
belongs x (S xs) = pertenece x xs
sizeS (S xs) = length (sinRepetir xs) -- O(n) + O(n^2) -> O(n^2)
removeS x (S xs) = S (sacar x xs)
unionS (S xs) (S ys) = S (xs ++ ys)
setToList (S xs) = sinRepetir xs