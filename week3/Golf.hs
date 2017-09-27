import Data.List  

{------- EX 1 -------}

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


{------- EX 2 -------}
localMaxima:: [Int] -> [Int]
localMaxima (x:y:[]) = []
localMaxima (x:y:z:xs) 
    | isMax y x z  = y : localMaxima (y:z:xs)
    | otherwise =  localMaxima (y:z:xs)


isMax::Int ->Int -> Int -> Bool
isMax x y z =  x > z && x > y 

{------- EX 3 -------}
{-histogram:: [Int] -> String-}
histogram list = foldl intoArray [newList] list 

{- [1,4,5,6,7,1] -}
{-[[1,0,0,1,1,1,1,0,0,0],[1,0,0,0,0,0,0,0,0,0]]-}
intoArray:: [[Int]] -> Int -> [[Int]]
intoArray acum position 
    | length filteredList == length acum  = acum ++ [(insertItemAt position newList)]
        {-lo tengo que insertar en el medio eso es lo que falla-}
    | length filteredList >= 1 && length filteredList < length acum = [(insertItemAt position (listAt (length filteredList) acum))]
    | otherwise = [insertItemAt position (last acum)]
    where filteredList = filter (founded position) acum
          founded position list = (==) 1 (listAt position list)


listAt :: Int -> [a] -> a
listAt position  = last . take position

newList = [0,0,0,0,0,0,0,0,0,0]

insertItemAt:: Int -> [Int] -> [Int]
insertItemAt position = aux . splitAt position   
    where aux (xs,ys) =  take (length xs - 1) xs ++ [1] ++ ys

