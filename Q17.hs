module Q17 where

split :: [a] -> Int -> ([a], [a])
split [] _ = ([],[])
split x 0 = ([], x)
split (x:xs) n = (x:fst(remaining), snd(remaining))
    where remaining = split xs (n-1)

runQ17 = do
    print $ split "abcdefg" 3
    print $ split "abcdefg" 1
    print $ split "abcdefg" 0
    print $ split "abcdefg" 100
