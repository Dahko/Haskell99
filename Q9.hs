module Q9 where

--last element block, remainder of the list, result
packHelp :: (Eq a) => [a] -> [a] ->[[a]]
packHelp [] [] = []
packHelp xs [] = [xs]
packHelp [] (x:xs) = packHelp [x] xs
packHelp (nx:nxs) (x:xs)
    | nx == x = packHelp (nx:nxs++[x]) xs
    | otherwise = [nx:nxs] ++ packHelp [] (x:xs)

pack :: (Eq a) => [a] -> [[a]]
pack = packHelp []

emptyStr :: String
emptyStr = []

runQ9 = do
    print $ pack "1233344555556"
    print $ pack "123456"
    print $ pack "1234566"
    print $ pack "1123456"
    print $ pack "11"
    print $ pack "1"
    print $ pack emptyStr
