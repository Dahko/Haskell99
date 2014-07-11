module Q18 where

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ start end | start > end = []
slice (x:xs) 1 1 = [x]
slice (x:xs) 1 end = x : slice xs 1 (end-1)
slice (x:xs) start end = slice xs (start-1) (end-1)

runQ18 = do
    print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
    print $ slice "abcdefghik" 1 1
    print $ slice "abcdefghik" 100 101
    print $ slice "abcdefghik" 6 1
