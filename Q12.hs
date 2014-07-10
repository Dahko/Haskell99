module Q12 where

data SinOrMul a = Single a | Multiple Int a deriving (Show)

decodeSOM :: SinOrMul a -> [a]
decodeSOM (Single x) = [x]
decodeSOM (Multiple n x) = replicate n x

decodeModified :: [SinOrMul a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeSOM x ++ decodeModified xs

emptySOM :: [SinOrMul Char]
emptySOM = []

runQ12 = do
    print $ decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] --"aaaabccaadeeee"
    print $ decodeModified [Multiple 4 'a']
    print $ decodeModified [Single 'a']
    print $ decodeModified emptySOM
