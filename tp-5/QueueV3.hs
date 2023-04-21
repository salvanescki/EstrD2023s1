module QueueV3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a] [a]
{- INV. REP: de Q fs bs
    * Si fs está vacía, la queue completa está vacía
-}

emptyQ :: Queue a                                             -- O(1)
isEmptyQ :: Queue a -> Bool                                   -- O(1)
enqueue :: a -> Queue a -> Queue a                            -- O(1)
firstQ :: Queue a -> a                                        -- O(n)
dequeue :: Queue a -> Queue a                                 -- O(n)

-- Implementación

emptyQ = Q [] []

isEmptyQ (Q [] []) = True
isEmptyQ _ = False

enqueue y (Q xs ys) = Q xs (y:ys)

firstQ (Q [] ys) = head (reverse ys)
firstQ (Q (x:_) _) = x

dequeue (Q [] []) = error "No se puede desencolar de una cola vacia"
dequeue (Q [] ys) = dequeue (Q (reverse ys) [])
dequeue (Q (x:xs) ys) = Q xs ys
