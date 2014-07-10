module Q13 where

data SinOrMul a = Single a | Multiple Int a deriving (Show)

-- number of elements=x so far, last element=x, list of all remaining elements
--encodeDirHlp :: (Eq a) => Int -> [a] -> [a] -> [(Int, a)]
encodeDirHlp :: (Eq a) => SinOrMul a -> [a] -> [SinOrMul a]
encodeDirHlp x [] = [x]
encodeDirHlp (Single x_old) (x:xs)
    | x_old == x = encodeDirHlp (Multiple 2 x_old) xs
    | otherwise = (Single x_old) : (encodeDirHlp (Single x) xs)
encodeDirHlp (Multiple n x_old) (x:xs)
    | x_old == x = encodeDirHlp (Multiple (n+1) x_old) xs
    | otherwise = (Multiple n x_old) : (encodeDirHlp (Single x) xs)

encodeDirect :: (Eq a) => [a] -> [SinOrMul a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirHlp (Single x) xs

runQ13 = do
    print $ encodeDirect "aaaabccaadeeee"
