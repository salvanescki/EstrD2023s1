module Set 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a]

emptyS :: Set a
addS :: Eq a => a -> Set a -> Set a
belongs :: Eq a => a -> Set a -> Bool
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
setToList :: Eq a => Set a -> [a]

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

agregarSiNoEsta :: Eq a => a -> Set a -> Set a
agregarSiNoEsta x (S xs) = if pertenece x xs then (S xs) else (S (x:xs))

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

unionL :: Eq a => [a] -> [a] -> [a]
unionL [] ys = ys
unionL (x:xs) ys = if pertenece x ys then unionL xs ys else x : unionL xs ys

emptyS = S []
addS x s = agregarSiNoEsta x s
belongs x (S xs) = pertenece x xs
sizeS (S xs) = length xs
removeS x (S xs) = S (sacar x xs)
unionS (S xs) (S ys) = S (unionL xs ys)
setToList (S xs) = xs