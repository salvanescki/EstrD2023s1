import Empresa --(Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado, agregarASector, borrarEmpleado)
import Empleado

{-

------------------------------------- Ejercicio 1 -----------------------------------------------

Considerando que al implementar heapsort, utilicé priorityQueueToList y listToProrityQueue, calculo sus costos (siempre en el peor caso):

* listToPriorityQueue: En mi implementación anterior, el insertPQ tenía costo constante (ya que solo hacía un cons)
  Sin embargo, ahora el costo de dicha operación es O(log n), siendo n la cantidad de elementos de la priorityQueue.
  Por lo que, listToPriorityQueue, para cada elemento de la lista, aplica una operación de costo O(log n),
  como estamos utilizando la misma cantidad de elementos en ambas estructuras, el costo resulta en O(n log n)

* priorityQueueToList: Se aplican las operaciones findMinPQ y deleteMinPQ a cada elemento de la priorityQueue.
  Si la implementación actual se trata de un Heap, podemos considerar que la raíz del mismo es el mínimo, esto significa
  que findMinPQ es de costo O(1). Además, segun la consigna, deleteMinPQ es de costo O(log n). priorityQueueToList
  aplica ambas operaciones por cada elemento en la PQ, por lo que su costo sería O(n log n)

Al heapsort aplicar ambas operaciones, su costo es O(n log n) mejorando de su anterior O(n^2)

-}

------------------------------------- Ejercicio 2 -----------------------------------------------

{- 
  Para todas las funciones siguientes, N refiere al número de elementos del árbol. Además el árbol es un BST y no tiene elementos repetidos.
  Para árboles no balanceados, el costo podría empeorar a O(n). Pero por cuestiones prácticas, todos los O(log N) en promedio serán tomados como si fueran el peor caso.
-}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

ejBST :: Tree Int 
ejBST = (NodeT 8 
            (NodeT 4
              (NodeT 2 
                (NodeT 1 EmptyT EmptyT) 
                (NodeT 3 EmptyT EmptyT)
              )
              (NodeT 6 
                (NodeT 5 EmptyT EmptyT) 
                (NodeT 7 EmptyT EmptyT)
              )
            )
            (NodeT 12
              (NodeT 10 
                (NodeT 9 EmptyT EmptyT)  
                (NodeT 11 EmptyT EmptyT)  
              )
              (NodeT 14 
                (NodeT 13 EmptyT EmptyT)  
                (NodeT 15 EmptyT EmptyT)  
              )
            )
          )

ejTree :: Tree Int
ejTree = (NodeT 1 
            (NodeT 2
              (NodeT 3 
                (NodeT 4 EmptyT EmptyT) 
                (NodeT 5 EmptyT EmptyT)
              )
              (NodeT 6 
                (NodeT 7 EmptyT EmptyT) 
                (NodeT 8 EmptyT EmptyT)
              )
            )
            (NodeT 9
              (NodeT 10 
                (NodeT 11 EmptyT EmptyT)  
                (NodeT 12 EmptyT EmptyT)  
              )
              (NodeT 13 
                (NodeT 14 EmptyT EmptyT)  
                (NodeT 15 EmptyT EmptyT)  
              )
            )
          )
ejDesbalanceado :: Tree Int
ejDesbalanceado = (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT) EmptyT)
-- Aux

isEmpty :: Tree a -> Bool
isEmpty EmptyT = True
isEmpty _ = False

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ EmptyT EmptyT) = 1
heightT (NodeT _ lt rt) = 1 + max (heightT lt) (heightT rt)

{- 
  Costo O(log N) ya que comprueba si el nodo actual es el que estamos buscando O(1), y luego verifica si el nodo actual es menor o mayor al que buscamos.
  Si es mayor, recorre el árbol derecho, sino el izquierdo (esto es gracias al invariante de BST que nos asegura que está ordenado). Como en cada llamado recursivo
  calcula si está en el nodo, si tiene que ir a su izquierda o a su derecha; belongsBST termina recorriendo tan solo una rama del árbol. Por lo tanto, su costo es O(log N).
-}

belongsBST :: Ord a => a -> Tree a -> Bool
-- PRECOND: El árbol es BST
belongsBST _ EmptyT = False
belongsBST x (NodeT x' lt rt) = x == x' || 
                                if x < x' then belongsBST x lt
                                else belongsBST x rt

{-
  Costo O(log N). Al igual que la anterior función, se apoya en que el árbol sea un BST para saber donde buscar el lugar en el que se debe insertar el elemento. Como a la izquierda
  son menores y a la derecha mayores, hace el recorrido de, como mucho, una rama del árbol.
-}                                

insertBST :: Ord a => a -> Tree a -> Tree a
-- PRECOND: El árbol es BST
insertBST x EmptyT = (NodeT x EmptyT EmptyT)
insertBST x (NodeT x' lt rt) = 
  if x == x' then (NodeT x lt rt)
  else if x < x' then (NodeT x' (insertBST x lt) rt)
                 else (NodeT x' lt (insertBST x rt))

{-
  Costo O(log N). Ya que recorre solo una rama del árbol O(log N) y, cuando encuentra el nodo aplica la operación rearmarBST que tiene costo operacional de O(log N)
-}   

deleteBST :: Ord a => a -> Tree a -> Tree a
-- PRECOND: El árbol es BST
deleteBST x EmptyT = EmptyT
deleteBST x (NodeT x' lt rt) =
  if x == x' then rearmarBST lt rt
  else if x < x' then (NodeT x' (deleteBST x lt) rt)
                 else (NodeT x' lt (deleteBST x rt))

{-
  Costo O(log N) ya que aplica splitMinBST de costo O(log N)
-}

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
-- PRECOND: Ambos árboles son BST
rearmarBST EmptyT rt = rt
rearmarBST lt rt = let (m, rt') = splitMinBST rt
                    in NodeT m lt rt'

{-
  Costo O(log N) ya que se recorre una rama para sacar el mínimo y devolver tanto el mínimo como el árbol sin él
-}

splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- PRECOND: el árbol es BST y no está vacío
splitMinBST (NodeT x EmptyT rt) = (x, rt)  
splitMinBST (NodeT x lt rt)     = let (m, lt') = splitMinBST lt
                                   in (m, NodeT x lt' rt)

{-
  Costo O(log N) ya que se recorre una rama para sacar el máximo y devolver tanto el máximo como el árbol sin él
-}

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- PRECOND: el árbol es BST y no está vacío
splitMaxBST (NodeT x lt EmptyT) = (x, lt)  
splitMaxBST (NodeT x lt rt)     = let (m, rt') = splitMaxBST rt
                                   in (m, NodeT x lt rt')

--
{-
  O(log N) ya que recorre una rama
-}
maxBST :: Ord a => Tree a -> a
-- PRECOND: el árbol no está vacío
maxBST (NodeT x _ EmptyT) = x
maxBST (NodeT _ _ rt) = maxBST rt

{-
  O(log N) ya que recorre una rama
-}
minBST :: Ord a => Tree a -> a
-- PRECOND: el árbol no está vacío
minBST (NodeT x EmptyT _) = x
minBST (NodeT _ lt _) = minBST lt

{-
  O(N) ya que revisa si el nodo pasado por parámetro es mayor a cada nodo del árbol
-}

esMayorA :: Ord a => a -> Tree a -> Bool
esMayorA _ EmptyT = True
esMayorA x' (NodeT x lt rt) = x > x' && esMayorA x' lt && esMayorA x' rt

{-
  O(N) ya que revisa si el nodo pasado por parámetro es menor a cada nodo del árbol
-}

esMenorA :: Ord a => a -> Tree a -> Bool
esMenorA _ EmptyT = True
esMenorA x' (NodeT x lt rt) = x < x' && esMenorA x' lt && esMenorA x' rt

{-
  O(N^2) ya que aplica 2 operaciones de costo lineal para cada nodo del árbol
-}

esBST :: Ord a => Tree a -> Bool
-- PRECOND: el árbol no tiene elementos repetidos
esBST EmptyT = True
esBST (NodeT x lt rt) = esMayorA x lt && esBST lt && esMenorA x rt && esBST rt

--
{-
  O(1) ya que solo hace una comparación entre datos
-}
elegirMaxEntre :: Ord a => a -> Maybe a -> Maybe a
elegirMaxEntre x Nothing = Just x
elegirMaxEntre x (Just x') = Just (max x x')

{-
  O(log N) ya que recorre solo un camino del árbol
-}
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA x' (NodeT x lt rt) = if x < x'
                                      then elegirMaxEntre x (elMaximoMenorA x' rt)
                                      else elMaximoMenorA x' lt
--

{-
  O(1) ya que solo hace una comparación entre datos
-}
elegirMinEntre :: Ord a => a -> Maybe a -> Maybe a
elegirMinEntre x Nothing = Just x
elegirMinEntre x (Just x') = Just (min x x')

{-
  O(log N) ya que recorre solo un camino del árbol
-}
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA _ EmptyT = Nothing
elMinimoMayorA x' (NodeT x lt rt) = if x' < x
                                      then elegirMinEntre x (elMinimoMayorA x' lt)
                                      else elMinimoMayorA x' rt
--
{-
  O(N^2) ya que aplica heightT (costo lineal) en cada nodo
-}
balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT x lt rt) = heightT lt - heightT rt <= 1 && balanceado lt && balanceado rt

------------------------------------- Ejercicio 3 -----------------------------------------------

{-
  De la consigna, los costos son:
  - emptyM .... O(1)
  - assocM .... O(log K)
  - lookupM ... O(log K)
  - deleteM ... O(log K)
  - keys ...... O(K)
  Siendo K la cantidad de claves del Map
-}

{- Aux

pertenece :: Eq a => a -> [a] -> Bool   -- O(n)
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

fromJust :: Maybe a -> a                -- O(1)
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

-}

{-
valuesK, por cada clave del Map (K claves) aplica lookupM, una operación de costo O(log K). Por lo que su costo es O(K log K)
valuesM, utiliza 1 vez la función keys, la cual cuesta O(K), y valuesK, O(K log K). Por lo que, valuesM tiene un costo de O(K log K).

valuesK :: Ord k => [k] -> Map k v -> [Maybe v]         
valuesK [] _ = []
valuesK (k:ks) mp = lookupM k mp : valuesK ks mp

valuesM :: Ord k => Map k v -> [Maybe v]                
valuesM mp = valuesK (keys mp) mp
-}

--
{-
Por cada elemento de la lista de claves ingresada por el Usuario ("m" claves), todasAsociadas llama a keys, costo O(K),
y a pertenece sobre la lista que devuelve keys (K claves), lo que también cuesta en el peor caso O(K).
Costo O(m * K) con m las claves de la lista ingresada por el Usuario y K la cantidad de claves del Map.
El peor caso, donde el Usuario ingresa una lista con todas las claves del Map sería O(K^2)

todasAsociadas :: Ord k => [k] -> Map k v -> Bool
todasAsociadas [] _ = True
todasAsociadas (k:ks) mp = pertenece k (keys mp) && todasAsociadas ks mp
-}
--
{-
Considerando la cantidad de pares de la lista como K, ya que luego va a ser la misma cantidad de claves del Map resultante,
listToMap llama assocM, función de costo O(log K), por cada llamado recursivo (el cual se hace K veces).
Esto resulta en un costo de O(K log K)

listToMap :: Ord k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)
-}
--
{-
Similar a lo anterior, la lista resultante va a tener la misma cantidad de claves que el Map (K claves).
mapToListK llama K veces a la función lookupM, de costo O(log K) y a fromJust, de costo O(1).
Por lo que mapToListK tiene un costo de O(K log K).
mapToList llama 1 vez a keys, costo O(K) y a mapToListK, costo O(K log K).
Por lo tanto, el costo de mapToList es de O(K log K)

mapToListK :: Ord k => [k] -> Map k v -> [(k, v)]
mapToListK [] _ = []
mapToListK (k:ks) mp = (k, fromJust(lookupM k mp)) : mapToListK ks mp

mapToList :: Ord k => Map k v -> [(k, v)]
mapToList mp = mapToListK (keys mp) mp
-}
--
{-
Al igual que en las dos funciones anteriores, la cantidad de pares de la lista es K, ya que va a resultar en K claves del Map.
Por cada par de la lista, agruparEq aplica:
* Siempre:
  * pertenece. Costo O(K)
  * keys. Costo O(K)
* Y aplica además o:
  * assocM. Costo O(log K)
* o:
  * assocM y lookupM, ambas de costo O(log K)
  * cons y fromJust O(1) ambas
Por lo tanto, para cada par de la lista (K pares), agruparEq aplica o:
* O(K) + O(K) + O(log K)
* O(K) + O(K) + O(log K) + O(log K) + O(1) + O(1)
agruparEq tiene un costo de O(K^2)

agruparEq :: Ord k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,v): kvs) = if pertenece k (keys (agruparEq kvs))
                            then assocM k (v : fromJust(lookupM k (agruparEq kvs))) (agruparEq kvs)
                            else assocM k [v] (agruparEq kvs)
-}
--
{-
Siendo m la cantidad de claves en la lista pasada por el Usuario,
incrementar aplica, por cada clave de esa lista (m claves): assocM, de costo O(log K); fromJust, O(1); y lookupM, O(log K).
Por lo tanto, tiene un costo de O(m log K). En el peor caso, que la lista tenga todas las claves del Map, su costo sería O(K log K).

incrementar :: Ord k => [k] -> Map k Int -> Map k Int
-- PRECOND: Las keys de la lista deben pertenecer al map
incrementar [] _ = emptyM
incrementar (k:ks) mp = assocM k (1 + fromJust(lookupM k mp)) (incrementar ks mp)
-}
--
{-
Siendo K la cantidad de claves del primer Map y K' la cantidad del segundo,
mergeMapsK aplica, por cada clave del primer Map (K claves): assocM, de costo O(log K); fromJust, O(1); y lookupM, O(log K).
En total un costo de O(K log K).
mergeMaps a su vez, aplica 1 vez keys en el primer Map, costo O(K) y mergeMapsK O(K log K).
Por lo tanto, mergeMaps tiene un costo de O(K log K)

mergeMapsK:: Ord k => [k] -> Map k v -> Map k v -> Map k v
mergeMapsK [] _ m2 = m2
mergeMapsK (k:ks) m1 m2 = assocM k (fromJust(lookupM k m1)) (mergeMapsK ks m1 m2)

mergeMaps:: Ord k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = mergeMapsK (keys m1) m1 m2
-}

--
{-
indexar toma una lista de elementos que pasa el Usuario y crea un Map con ellos, para comodidad digo que son K elementos.
En el proceso, para cada elemento de la lista (K elementos), aplica: assocM, O(log K); keys, O(K); y length sobre la lista que devuelve keys (K elementos), O(K).
Como aplica estas operaciones para cada elemento de la lista, indexar tiene un costo de O(K^2).

indexar :: [a] -> Map Int a
indexar [] = emptyM
indexar (x:xs) = assocM (length (keys(indexar xs))) x (indexar xs)
-}

{-
Al igual que en la función anterior, K es la cantidad de caracteres del String pasado por el Usuario.
Suponiendo que el String de K caracteres, sin repetidos tiene n caracteres con n <= K:
ocurrenciasS, para cada caracter del String sin repetidos (n chars), aplica: apariciones sobre el String original, costo O(K); assocM sobre el Map que, a su vez,
tiene la cantidad de caracteres del String sin repetidos (n chars), costo O(log n).
Por lo tanto, ocurrenciasS tiene un costo de O(n * K)
sinRepetidosS aplica, por cada caracter del String (n chars) addS, la cual su costo depende de la implementación de Set que se use, (mínimo O(1), máximo O(n))
sinRepetidos aplica 1 vez setToList, O(1) y sinRepetidosS que puede variar entre O(1) y O(n)
ocurrencias, aplica 1 vez ocurrenciasS, de costo O(n * K), y sinRepetidos, de costo dependiente de la implementación de Set (O(1)~O(n))
En todos los casos, ocurrencias tiene un costo de O(n * K).

unoSiCeroSino :: Bool -> Int -- O(1)
unoSiCeroSino True = 1
unoSiCeroSino False = 0

apariciones :: Eq a => a -> [a] -> Int -- O(n)
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

-}

------------------------------------- Ejercicio 5 -----------------------------------------------

type SectorId = Int
type CUIL = Int

{-
  La cant de SectorIDs en la lista es S, la cant de CUILs en la lista es E

  agregarSectores, por cada Sector de la lista aplica agregarSector, la cual tiene un costo O(log S)
  agregarEmpleados, por cada CUIL de la lista aplica agregarEmpleado. Como la lista de CUILs termina teniendo la misma longitud que
  la cant de Empleados la Empresa, el costo de agregarEmpleados es O(E log S + E log E)
  comenzarCon, al aplicar ambas funciones a una empresa vacía, tiene un costo de O(E log S + E log E)
-}
agregarSectores :: [SectorId] -> Empresa -> Empresa
-- OBSERVACIÓN: Los sectores no tienen empleados
agregarSectores [] e = e
agregarSectores (s:ss) e = agregarSector s (agregarSectores ss e)

agregarEmpleados :: [CUIL] -> Empresa -> Empresa
-- OBSERVACIÓN: Los empleados no tienen sectores asignados
agregarEmpleados [] e = e
agregarEmpleados (c:cs) e = agregarEmpleado [] c (agregarEmpleados cs e)

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
-- PRECOND: Ninguna de las listas es vacía
comenzarCon ss cs = agregarEmpleados cs (agregarSectores ss consEmpresa)

{-
  borrarEmpleado tiene un costo de O(M log S + M log E), siendo M la cant de sectores en las que trabajó el empleado, S los Sectores y E los Empleados.
  borrarNEmpleados llama borrarEmpleado las N veces que le indiquemos por parámetro, su costo sería de O(N * M (log S + log E))
  recorteDePersonal llama: 
  * borrarNEmpleados con la mitad de los Empleados, es decir E/2
  * length sobre todos los Empleados, O(E)
  * todosLosCUIL, O(E) 2 veces
  En total, su costo es O(3*E + E/2 * M (log S + log E)). Podría mejorarse si mejoro borrarEmpleado que es la función que más consume
-}
borrarNEmpleados :: Int -> [CUIL] -> Empresa -> Empresa
borrarNEmpleados 0 _ e = e
borrarNEmpleados n (c:cs) e = borrarEmpleado c (borrarNEmpleados (n - 1) cs e)

recorteDePersonal :: Empresa -> Empresa
recorteDePersonal e = borrarNEmpleados (div (length (todosLosCUIL e)) 2) (todosLosCUIL e) e

{-
  todosLosSectores tiene costo O(S)
  agregarEmpleado por otra parte, tiene costo O(S log S + S log E) ya que lo estamos agregando a todos los Sectores (S)
  Por lo tanto, el costo total es O(S + S log S + S log E), o también O(S * (1 + log S + log E)) un eneloguene
-}
convertirEnComodin :: CUIL -> Empresa -> Empresa
-- OBSERVACIÓN: Considero que agregar el Empleado, pisa los datos antiguos
convertirEnComodin c e = agregarEmpleado (todosLosSectores e) c e

{-
  buscarPorCUIL tiene costo O(log E), sectores
-}
esComodin :: CUIL -> Empresa -> Bool
esComodin c e = let empleado = (buscarPorCUIL c e)
                 in sectores empleado == todosLosSectores e
