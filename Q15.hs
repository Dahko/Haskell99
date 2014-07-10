module Q15 where

repOne :: a -> Int -> [a]
repOne _ 0 = []
repOne x n = x:repOne x (n-1)

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (repOne x n) ++ (repli xs n)

runQ15 = do
    print $ repli "123" 5
