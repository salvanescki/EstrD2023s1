import PriorityQueue -- PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ
import MapV3 -- Map, emptyM, assocM, lookupM, deleteM, keys
import MultiSet -- MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList
import SetV1
--

-- O(n^2) ya que aplica findMinPQ y deleteMinPQ [ambas O(n)] para cada elemento en la PQ (misma cantidad que la lista)
priorityQueueToList :: Ord a => PriorityQueue a -> [a]
priorityQueueToList pq = if isEmptyPQ pq then [] else findMinPQ pq : priorityQueueToList (deleteMinPQ pq)

-- O(n) siendo n la cantidad de elementos de la lista
listToPriorityQueue :: Ord a => [a] -> PriorityQueue a
listToPriorityQueue [] = emptyPQ
listToPriorityQueue (x:xs) = insertPQ x (listToPriorityQueue xs)

-- O(n^2)
heapSort :: Ord a => [a] -> [a]
heapSort xs = priorityQueueToList (listToPriorityQueue xs)

--

ejMap :: Map String Int
ejMap = assocM "4" 4
        $ assocM "3" 3
        $ assocM "2" 2
        $ assocM "a" 1
        $ emptyM

ejMap2 :: Map String Int
ejMap2 = assocM "d" 8
        $ assocM "c" 7
        $ assocM "b" 6
        $ assocM "a" 5
        $ emptyM

pertenece :: Eq a => a -> [a] -> Bool -- O(n)
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

fromJust :: Maybe a -> a -- O(1)
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

--
{-
Aplica lookupM a cada elemento de la lista de claves.
Considerando que dicha lista contiene las claves del Map, tienen el mismo tamaño.
Por lo que, lookupM en el peor caso debe costar O(n) con n la cantidad de claves del Map
En dicho caso, valuesK costaría O(n^2).
valuesM utiliza 1 vez la función keys, la cual costaría O(n) en el peor caso. Por lo que, valuesM O(n^2)
-}

valuesK :: Ord k => [k] -> Map k v -> [Maybe v]         
valuesK [] _ = []
valuesK (k:ks) mp = lookupM k mp : valuesK ks mp

valuesM :: Ord k => Map k v -> [Maybe v]                
valuesM mp = valuesK (keys mp) mp

--
{-
Por cada elemento de la lista de claves ("m" claves) ingresada por el Usuario, la función llama a keys del Map (Costo probable O(n))
y a pertenece sobre esa lista resultante del Map, O(n) con n la cantidad de claves dentro del Map.
Costo O(n * m) con n las claves del Map y m las claves de la lista ingresada por el Usuario.
El peor caso, donde el Usuario ingresa una lista con todas las claves del Map sería O(n^2)
-}
todasAsociadas :: Ord k => [k] -> Map k v -> Bool
todasAsociadas [] _ = True
todasAsociadas (k:ks) mp = pertenece k (keys mp) && todasAsociadas ks mp

--
{-
El costo de esta función depende del costo que tenga la implementación de assocM.
Como aplica assocM por cada elemento par de la lista de pares ("n" pares),
el costo debería ser n veces el costo de assocM.
Ej: Como el Map en un principio está vacío y se va llenando a medida que se recorre la lista
    el costo (si assocM cuesta O(n)) sería O(n^2)
-}
listToMap :: Ord k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

--
{-
Otra vez, el costo depende de una función de la interfaz.
Dependiendo del costo de lookupM, mapToListK tiene que aplicarla n veces
con n la cantidad de claves del Map. El peor caso es O(n^2)
-}
mapToListK :: Ord k => [k] -> Map k v -> [(k, v)]
mapToListK [] _ = []
mapToListK (k:ks) mp = (k, fromJust(lookupM k mp)) : mapToListK ks mp

mapToList :: Ord k => Map k v -> [(k, v)]
mapToList mp = mapToListK (keys mp) mp

--
ejLista = [
            ("a", 1),
            ("a", 2),
            ("a", 3),
            ("b", 4),
            ("b", 5),
            ("c", 6)
          ]
-- { "a":[1,2,3], "b":[4,5], "c":[6] }
{-
Por cada par de la lista, agruparEq aplica:
Siempre:
pertenece. Costo O(n) con n la cantidad de claves que tenga en ese momento el llamado recursivo
keys. Depende del costo de la implementación (prob O(n))
Y aplica además o:
assocM. Depende del costo de la implementación (prob O(n))
o:
assocM y lookupM (prob O(n) + O(n))
cons y fromJust O(1) ambas
Por lo tanto, en cada par de lista (siendo n pares) aplica o: O(n)+"O(n)"+"O(n)" o O(n)+"O(n)"+"O(n)"+"O(n)".
Costo O(n^2)
-}
agruparEq :: Ord k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,v): kvs) = if pertenece k (keys (agruparEq kvs))
                            then assocM k (v : fromJust(lookupM k (agruparEq kvs))) (agruparEq kvs)
                            else assocM k [v] (agruparEq kvs)

--
{-
Depende del costo de assocM. O(m * n) con assocM O(n) y m claves de la lista, n claves del Map (m <= n)
-}
incrementar :: Ord k => [k] -> Map k Int -> Map k Int
-- PRECOND: Las keys de la lista deben pertenecer al map
incrementar [] _ = emptyM
incrementar (k:ks) mp = assocM k (1 + fromJust(lookupM k mp)) (incrementar ks mp)

--
{-
Lo mismo que en el análisis anterior. Siendo cant de claves de m1: n, y cant de claves de m2: m,
Se aplica, por cada clave de m1 (n), assocM (costo prob O(m)) y lookupM (costo prob O(n)).
Costo O(n*m) o O(n^2)
-}
mergeMapsK:: Ord k => [k] -> Map k v -> Map k v -> Map k v
mergeMapsK [] _ m2 = m2
mergeMapsK (k:ks) m1 m2 = assocM k (fromJust(lookupM k m1)) (mergeMapsK ks m1 m2)

mergeMaps:: Ord k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = mergeMapsK (keys m1) m1 m2

--
{-

indexar :: [a] -> Map Int a

Se me ocurren dos formas de hacer este:
- Con doble PM, llamando a una subtarea que se le pase el length xs y la lista, y que vaya reduciendo el numero de length xs a 0 (siendo estos los index)
- Con length xs en cada iteración. Este cuesta un poco más ya que toma el length de lo que devuelve la recursión. Hace básicamente esto:
        coste n, coste n-1, ..., coste 3, coste 2, coste 1
  por lo que es un O(n^2) rebajadísimo, pero no me gusta tanto como la de arriba que es O(n)


-}

-- O(n^2)
indexar :: [a] -> Map Int a
indexar [] = emptyM
indexar (x:xs) = assocM (length (keys(indexar xs))) x (indexar xs)

-- O(n^2)

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiCeroSino(e == x) + apariciones e xs

sinRepetidosS :: Eq a => [a] -> Set a
sinRepetidosS [] = SetV1.emptyS
sinRepetidosS (x:xs) = addS x (sinRepetidosS xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (sinRepetidosS xs)

ocurrenciasS :: String -> String -> Map Char Int
ocurrenciasS [] _ = emptyM
ocurrenciasS (c:cs) s = assocM c (apariciones c s) (ocurrenciasS cs s)

ocurrencias :: String -> Map Char Int
ocurrencias cs = ocurrenciasS (sinRepetidos cs) cs

--

ocurrenciasMS :: String -> MultiSet Char
ocurrenciasMS [] = emptyMS
ocurrenciasMS (c:cs) = addMS c (ocurrenciasMS cs)
