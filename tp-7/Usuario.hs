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

belongsBST :: Ord a => a -> Tree a -> Bool

insertBST :: Ord a => a -> Tree a -> Tree a

deleteBST :: Ord a => a -> Tree a -> Tree a

splitMinBST :: Ord a => Tree a -> (a, Tree a)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)

esBST :: Tree a -> Bool

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a

balanceado :: Tree a -> Bool
