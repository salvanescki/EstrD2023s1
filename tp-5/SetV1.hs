module SetV1 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] Int
{- INV.REP: en (Set xs n)
    * xs no tiene elementos repetidos
    * n es la longitud de la lista
-}

emptyS :: Set a                                             -- O(1)
addS :: Eq a => a -> Set a -> Set a                         -- O(n)
belongs :: Eq a => a -> Set a -> Bool                       -- O(n)
sizeS :: Eq a => Set a -> Int                               -- O(1)
removeS :: Eq a => a -> Set a -> Set a                      -- O(n)
unionS :: Eq a => Set a -> Set a -> Set a                   -- O(n^2)
setToList :: Eq a => Set a -> [a]                           -- O(1)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

agregarSiNoEsta :: Eq a => a -> Set a -> Set a
agregarSiNoEsta x (S xs n) = if pertenece x xs then (S xs n) else (S (x:xs) (n+1))

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

unionL :: Eq a => [a] -> [a] -> [a]
unionL [] ys = ys
unionL (x:xs) ys = if pertenece x ys then unionL xs ys else x : unionL xs ys

emptyS = S [] 0
addS x s = agregarSiNoEsta x s
belongs x (S xs n) = pertenece x xs
sizeS (S xs n) = n
removeS x (S xs n) = S (sacar x xs) (length xs) -- O(2n) -> O(n)
unionS (S xs n1) (S ys n2) = S (unionL xs ys) (length (unionL xs ys)) -- #xs + #ys - #(xs n ys)
setToList (S xs n) = xs