{-EXERCISE ONE-} 
decompose :: Integer -> [Integer]
decompose number 
    | (<=) number 0  = []
    | otherwise     = lastDigit number : (decompose . removeLastDigit ) number

lastDigit :: Integer -> Integer
lastDigit number = mod number 10

removeLastDigit :: Integer -> Integer
removeLastDigit number = div ((-) number (lastDigit number)) 10 

toDigitsRev :: Integer -> [Integer]
toDigitsRev = decompose 

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

{-EXERCISE TWO -}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =  (reverse . doubleEvenPosition . reverse) xs

doubleEvenPosition :: [Integer] -> [Integer]
doubleEvenPosition [] = []
doubleEvenPosition (x:[]) = x:[]
doubleEvenPosition (x:y:xs) = x : (*2) y : doubleEvenPosition xs

{-EXERCISE 3-}
sumDigits [] = 0 
sumDigits (x:xs) = (+) (sumDigitsNumber x) (sumDigits xs) 

sumDigitsNumber :: Integer -> Integer
sumDigitsNumber = sum . toDigits

{-EXERCISE 4 -}
eq0 :: Integer -> Bool
eq0 = (== 0) 

flipLazy :: (a -> b -> c) -> (b -> a -> c) 
flipLazy f = g 
    where g x y = f y x 

flippedMod :: Integer -> Integer -> Integer
flippedMod = flipLazy mod

validate :: Integer -> Bool
validate  =  ( eq0 . (flippedMod 10) . sumDigits. doubleEveryOther . toDigits ) 

