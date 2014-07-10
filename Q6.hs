module Q6 where

isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) 
    | x == lastEl = isPalindrome revRemainder
    | otherwise = False
    where (lastEl:revRemainder) = reverse xs

runQ6 = do
    print $ isPalindrome "abba"
    print $ isPalindrome "abc"
    print $ isPalindrome "abca"
    print $ isPalindrome "madamimadam"
    print $ isPalindrome "madrmimadam"
