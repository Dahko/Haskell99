module Q20 where

removeNth :: [a] -> Int -> [a]
removeNth [] _ = []
removeNth (x:xs) n 
    | n == 1 = removeNth xs (n-1)
    | otherwise = x : removeNth xs (n-1)

-- NB solution differs from the question a bit, e.g. wee need to return the element,
-- but this is quite trivial using myNth function or the same trick with tuples as in Q19

runQ20 = do
    print $ removeNth "abcd" 2
    print $ removeNth "abcd" 8
    print $ removeNth "abcd" (-1)
    print $ removeNth "abcd" 1
    print $ removeNth "abcd" 0
