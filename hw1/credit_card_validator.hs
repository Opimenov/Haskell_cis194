import Data.List

{-given a list and a number adds digits
contained in a number to the list-}
getNums :: Integral a => a -> t -> [a]
getNums n x
  | n == 0 = []
  | otherwise =(n `mod` 10) :  getNums (n `quot` 10) x

{-given a number returns a reversed list of digits
contained in a number. 321 -> [1,2,3]-}
toDigitsRev :: Integral a => a -> [a]
toDigitsRev x
  | x <= 0    = []
  | otherwise = getNums x []

{-given a number returns a list of digits
contained in a number. 123 -> [1,2,3]-}
toDigits :: Integral a => a -> [a]
toDigits x = reverse $ toDigitsRev x

{-given a list, creates a list of tuples,
where first element is element of a list
and second is an numerical index starting from the end
[12,13,14] -> [(12,3),(13,2),(14,1)]-}
addReverseIndecies :: [a] -> [(a, Int)]
addReverseIndecies x = zip x $ reverse $ [1..(length x)]

{-given a list of numerical values, doubles every other
element starting from the end-}
doubleEveryOther :: (Integral b, Num t) => [(t, b)] -> [t]
doubleEveryOther xs = [if even $ snd x then fst x *2 else fst x | x <- xs]

{-given a list of numbers returns a list of digits
[1,22,45,9] -> [1,2,2,4,5,9]-}
unrollAllDigits :: Integral t => [t] -> [t]
unrollAllDigits xs = [ y | x <- xs, y <- toDigits x]

{-given a list of numbers sums all digits
[1,23,4] -> 1+2+3+4 = 10-}
sumDigits :: Integral a => [a] -> a
sumDigits x =  sum $ unrollAllDigits x

{-given a number calculates a checksum-}
getCheckSum :: Integral a => a -> a
getCheckSum x = sumDigits $
                unrollAllDigits $
                doubleEveryOther $
                addReverseIndecies $
                toDigits x

validate :: Integral a => a -> Bool
validate x = getCheckSum x `mod` 10 == 0
