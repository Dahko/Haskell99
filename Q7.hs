module Q7 where

data NestedList a = Elem a | List [NestedList a]

flattenList :: [NestedList a] -> [a]
flattenList [] = []
flattenList (x:xs) = (flatten x) ++ (flattenList xs) 

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = flattenList xs

emptyList :: [NestedList Char]
emptyList = []

runQ7 = do
    print $ flatten (Elem 5)
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    print $ flatten (List emptyList)
