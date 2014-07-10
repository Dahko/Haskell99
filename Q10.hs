module Q10 where

import Q9

-- get a list of lists and return list of tuples (n, x),
-- where n is a number of elements in sublist, x is the first element of sublist
encodePacked :: (Eq a) => [[a]] -> [(Int, a)]
encodePacked [] = []
encodePacked (x:xs) = (length x, head x) : encodePacked xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode = encodePacked . pack

runQ10 = do
    print $ encode "aaabbcddddef"
