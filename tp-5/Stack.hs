module Stack 
    (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where

data Stack a = S [a] Int

emptyS :: Stack a                                             -- O(1)
isEmptyS :: Stack a -> Bool                                   -- O(1)
push :: a -> Stack a -> Stack a                               -- O(1)
top :: Stack a -> a                                           -- O(1)
pop :: Stack a -> Stack a                                     -- O(1)
lenS :: Stack a -> Int                                        -- O(1)

-- Implementación

emptyS = S [] 0

isEmptyS (S [] 0) = True
isEmptyS _ = False

push x (S xs n) = S (x:xs) (n+1)
top (S xs n) = head xs
pop (S xs n) = S (tail xs) (n-1)
lenS (S xs n) = n