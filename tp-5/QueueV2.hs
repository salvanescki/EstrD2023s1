module QueueV2
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a]

emptyQ :: Queue a                                             -- O(1)
isEmptyQ :: Queue a -> Bool                                   -- O(1)
enqueue :: a -> Queue a -> Queue a                            -- O(1)
firstQ :: Queue a -> a                                        -- O(n)
dequeue :: Queue a -> Queue a                                 -- O(n)

-- Implementación

emptyQ = Q []

isEmptyQ (Q []) = True
isEmptyQ _ = False

enqueue x (Q xs) = Q (x:xs)
firstQ (Q xs) = last xs
dequeue (Q xs) = Q (init xs)