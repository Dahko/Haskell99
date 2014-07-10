module Q16 where

dropHlp :: [a] -> Int -> Int -> [a]
dropHlp [] _ _ = []
dropHlp (x:xs) per remaining
    | remaining == 0 = dropHlp xs per (per-1)
    | otherwise = x : (dropHlp xs per (remaining-1))

-- drop every Nth element of a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x 0 = x -- nothing to drop
dropEvery _ 1 = [] -- drop everything
dropEvery x n = dropHlp x n (n-1)

runQ16 = do
    print $ dropEvery "abcdefghjk" 2
    print $ dropEvery "abcdefghjk" 3
    print $ dropEvery "abcdefghjk" 4
    print $ dropEvery "abcdefghjk" 1
    print $ dropEvery "abcdefghjk" 0
