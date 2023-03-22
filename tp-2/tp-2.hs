-- Funciones de la práctica 1 que utilizo

sucesor :: Int -> Int
sucesor n = n + 1

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

------------------------------------Recursión sobre Listas------------------------------------
-- 1

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

-- 2

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 3

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = sucesor n : sucesores ns

-- 4

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (p:ps) = p && conjuncion ps

-- 5

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (p:ps) = p || disyuncion ps

-- 6

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (l:ls) = l ++ aplanar ls

-- 7

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- 8

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiCeroSino(e == x) + apariciones e xs

-- 9

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA k (n:ns) = if n < k
                        then n : losMenoresA k ns
                        else losMenoresA k ns

-- 10

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (l:ls) = if longitud l > n
                                then l : lasDeLongitudMayorA n ls
                                else lasDeLongitudMayorA n ls

-- 11

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = e : []
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- 12

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

-- 13

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

-- 14

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (x:xs) (y:ys) = if x > y
                            then x : zipMaximos xs ys
                            else y : zipMaximos xs ys

-- 15

elMinimo :: Ord a => [a] -> a
-- PRECOND: La lista no está vacía
elMinimo [x] = x
elMinimo (x:xs) = if x < elMinimo xs
                    then x
                    else elMinimo xs

------------------------------------Recursión sobre Números------------------------------------

-- 1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 2

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n - 1)

-- 3

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n - 1) e

-- 4

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n - 1) xs

-- 5

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 l = l
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n - 1) xs

