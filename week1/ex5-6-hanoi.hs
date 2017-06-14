type Peg = String
type Move = (Peg, Peg) 

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n source aux destination = hanoi ((-) n 1) source destination aux ++ (source,destination):[] ++ hanoi ((-) n 1 ) aux source destination 

