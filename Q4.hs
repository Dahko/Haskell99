module Q4 where

mylength [] = 0
mylength (x:xs) = 1 + mylength xs

runQ4 = do
    print $ mylength "1234567890"
    print $ mylength []
