module Q1 where

mylast :: [a] -> a
mylast [] = error "Empty list"
mylast (x:[]) = x 
mylast (x:xs) = mylast xs

runQ1 = do
    print $ mylast [1,2,3,4]
    print $ mylast "abcd"
    print $ mylast ""
