------------------------------------Cálculo de Costos--------------------------------------
{-

head' O(1)
sumar O(1)
factorial O(n)
longitud O(n)
factoriales O(n^2)
pertenece O(n)
sinRepetidos 
append 

-}
-- O(1)
head' :: [a] -> a
head' (x:xs) = x

-- O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- O(n) siendo n el número que queremos calcular su factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- O(n) ya que aplica una operacion de costo constante a cada elemento de la lista de longitud n
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- O(n^2) ya que aplica una operación de costo lineal (factorial) a cada elemento de una lista
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- O(n) siendo n la longitud de la lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- O(n^2) ya que para cada elemento de la lista comprueba si pertenece a la cola (costo lineal)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs

-- O(n) siendo n la longitud de la primera lista
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- O(n^2) ya que ++ es equivalente a hacer append (costo lineal) y esta función lo hace para cada string en la lista
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- O(n) siendo n la cantidad de elementos que queremos tomar de la lista
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- O(n) siendo n el n que pasamos como argumento
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

-- O(n) siendo n la longitud de la lista
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

-- O(n). No se el coste de min, pero como compara dos elementos será constante. Al aplicarse a cada elemento de la lista una operación constante, resulta en un costo lineal.
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- O(n) ya que el peor caso es que el elemento se encuentre en la última posición de la lista y, por lo tanto, haya que recorrerla completa.
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

-- O(n^2) ya que aplica minimo (costo lineal) a cada elemento de la lista.
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
    let m = minimo xs
        in m : ordenar (sacar m xs)