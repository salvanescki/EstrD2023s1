{-

Ejercicio 1:

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

-- Ejercicio 2

{- 
  Para todas las funciones siguientes, N refiere al número de elementos del árbol. Además el árbol es un BST y no tiene elementos repetidos.
  Para árboles no balanceados, el costo podría empeorar a O(n). Pero por cuestiones prácticas, todos los O(log N) en promedio serán tomados como si fueran el peor caso.
-}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

ejTree :: Tree Int 
ejTree = (NodeT 8 
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
{-
maxBST :: Ord a => Tree a -> a
-- PRECOND: el árbol no está vacío
maxBST (NodeT x _ EmptyT) = x
maxBST (NodeT _ _ rt) = maxBST rt

minBST :: Ord a => Tree a -> a
-- PRECOND: el árbol no está vacío
minBST (NodeT x EmptyT _) = x
minBST (NodeT _ lt _) = minBST lt

esBST :: Ord a => Tree a -> Bool
-- PRECOND: el árbol no tiene elementos repetidos
esBST EmptyT = True
esBST (NodeT x lt rt) = x > maxBST lt && x < minBST rt && esBST lt && esBST rt
-}
{-
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a

balanceado :: Tree a -> Bool
-}