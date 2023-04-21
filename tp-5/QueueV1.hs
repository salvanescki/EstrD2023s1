module QueueV1 
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a]

emptyQ :: Queue a                                             -- O(1)
isEmptyQ :: Queue a -> Bool                                   -- O(1)
enqueue :: a -> Queue a -> Queue a                            -- O(n)
firstQ :: Queue a -> a                                        -- O(1)
dequeue :: Queue a -> Queue a                                 -- O(1)

-- Aux

agregarAlFinal :: [a] -> a -> [a]   -- O(n)
agregarAlFinal [] e = e : []
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- Implementación

emptyQ = Q []

isEmptyQ (Q []) = True
isEmptyQ _ = False

enqueue x (Q xs) = Q (agregarAlFinal xs x)
firstQ (Q xs) = head xs
dequeue (Q xs) = Q (tail xs)
