import Data.List  

{-Using a map, to convert the values. every function will receive the whole list and one position (lazily) till the lenght of the list-}
skips :: [a] -> [[a]]
skips list  = map (every list) [1..length list] 


{-removing the elements before n, concat that element in the result list, 
 - then remove it and run the same process till the list have one element or when it's empty-}
every :: [a] -> Int -> [a]
every list n 
    | length list > n = head reducedList : every (drop 1 reducedList) n
    | otherwise = reducedList
    where reducedList = drop (n-1) list





