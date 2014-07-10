module Q14 where

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

runQ14 = do
    print $ dupli "123"
