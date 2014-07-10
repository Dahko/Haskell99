module Q2 where

my2last :: [a] -> a
my2last [] = error "Empty list"
my2last (x:[]) = error "Too short list" 
my2last (x:_:[]) = x 
my2last (x:y:xs) = my2last (y:xs)

runQ2 = do
    print $ my2last [1,2,3,4]
    print $ my2last "abcd"
    --print $ my2last "1"
    --print $ my2last ""
