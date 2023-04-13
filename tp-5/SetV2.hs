module SetV2 
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

sinRepetir :: Eq a => [a] -> [a]
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
sizeS (S xs) = length (sinRepetir xs)
removeS x (S xs) = S (sacar x xs)
unionS (S xs) (S ys) = S (xs ++ ys)
setToList (S xs) = sinRepetir xs