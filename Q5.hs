module Q5 where

myRev :: [a] -> [a]
myRev [] = []
myRev (x:xs) = (myRev xs) ++ [x]

emptyStr :: String
emptyStr = []

runQ5 = do
    print $ myRev [1,2,3,4]
    print $ myRev "abcd"
    print $ myRev emptyStr
    
