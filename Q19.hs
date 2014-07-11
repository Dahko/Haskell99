module Q19 where

-----------
--variant 1
    
tail' :: [a] -> a
tail' [] = undefined
tail' (x:[]) = x
tail' (x:xs) = tail' xs

cuttail :: [a] -> [a]
cuttail [] = []
cuttail (x:[]) = []
cuttail (x:xs) = x:cuttail(xs)

-- prepends last el of the list to the list (and removes it from the end of the list)
prependtail :: [a] -> [a]
prependtail [] = []
prependtail xs = tail'(xs) : cuttail(xs)

------------
-- variant 2

-- splits array into all but tail and tail
splittail :: [a] -> ([a],[a])
splittail [] = ([],[])
splittail (x:[]) = ([], [x])
splittail (x:xs) = (x:sublist, subtail)
    where (sublist, subtail) = splittail xs

prependtail' :: [a] -> [a]
prependtail' [] = []
prependtail' xs = head(mytail):myremainder
    where (myremainder, mytail) = splittail xs

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n
    | n > 0 = rotate (xs++[x]) (n-1)
    | n < 0 = rotate (prependtail'(x:xs)) (n+1)
--    | n < 0 = rotate (prependtail(x:xs)) (n+1)


runQ19 = do
    print $ rotate ['a','b','c','d','e','f','g','h'] 3
    print $ rotate ['a','b','c','d','e','f','g','h'] (-2)
