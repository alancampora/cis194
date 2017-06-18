{-EXERCISE 5 -}
type Peg = String
type Move = (Peg, Peg) 

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n source aux destination = hanoi ((-) n 1) source destination aux ++ (source,destination):[] ++ hanoi ((-) n 1 ) aux source destination 

amountMovementsHanoi :: Int -> Int
amountMovementsHanoi n = length (hanoi n "a" "b" "c")


{-EXERCISE 6-}
top :: Int -> Int
top n = quot n 2 

bottom :: Int -> Int
bottom n =  (n - top n ) - 1


hanoi4Pegs :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4Pegs 0 _ _ _ _ = []
hanoi4Pegs n source aux1 aux2 destination = 
    hanoi4Pegs (top n) source aux2 destination aux1 ++ hanoi4Pegs (bottom n) source aux1 destination aux2 
    ++ (source,destination):[]
    ++ hanoi4Pegs (bottom n) aux2 aux1 source destination ++ hanoi4Pegs (top n) aux1 aux2 source destination 


amountMovementsHanoi4Pegs :: Int -> Int
amountMovementsHanoi4Pegs n = length (hanoi4Pegs n "a" "b" "c" "d")

