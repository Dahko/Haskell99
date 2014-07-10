module Q11 where

import Q10

data SinOrMul a = Single a | Multiple Int a deriving (Show)

encodeHlp :: (Eq a) => [(Int, a)] -> [SinOrMul a]
encodeHlp [] = []
encodeHlp (x:xs) 
    | fst x == 1 = Single (snd x) : encodeHlp xs
    | otherwise = Multiple (fst x) (snd x) : encodeHlp xs

encodeModified :: (Eq a) => [a] -> [SinOrMul a]
encodeModified [] = []
encodeModified x = encodeHlp (encode x)

runQ11 = do
    print $ encodeModified "aaaabccaadeeee"
    print $ encodeModified "aaaa"
    print $ encodeModified "a"
    print $ encodeModified ""
