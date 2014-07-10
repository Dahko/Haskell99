module Q3 where

myNth :: (Num b, Ord b) => [a] -> b -> a
myNth _ n | n<=0 = error "Index should be >=1"
myNth [] _ = error "List too short"
myNth (x:xs) 1 = x
myNth (x:xs) n = myNth xs (n-1)

runQ3 = do
    print $ myNth [1,2,3,4] 2
    print $ myNth "abcd" 3
    --print $ myNth "abcd" 8
    --print $ myNth "abcd" (-1)
    --print $ myNth "" 1
