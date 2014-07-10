module Q8 where

-- last value, than the list remainder
helper :: (Eq a) => [a] -> [a] -> [a]
helper [] (x:xs) = x : helper [x] xs
helper (old:_) (x:xs)
    | old == x = helper [x] xs
    | otherwise = x : helper [x] xs
helper _ [] = []

compress :: (Eq a) => [a] -> [a]
compress = helper [] 

runQ8 = do
    print $ compress "aaaaabcccddaa"
