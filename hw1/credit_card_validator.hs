import Data.List

getNums :: Integral a => a -> t -> [a]
getNums n x
  | n == 0 = []
  | otherwise =(n `mod` 10) :  getNums (n `quot` 10) x


toDigitsRev :: Integral a => a -> [a]
toDigitsRev x
  | x <= 0    = []
  | otherwise = getNums x []

  
toDigits :: Integral a => a -> [a]
toDigits x = reverse $ toDigitsRev x

addReverseIndecies :: [a] -> [(a, Int)]
addReverseIndecies x = zip x $ reverse $ [1..(length x)]

doubleEveryOther :: (Integral b, Num t) => [(t, b)] -> [t]
doubleEveryOther xs = [if even $ snd x then fst x *2 else fst x | x <- xs]

sumDigits :: Num a => [a] -> a
sumDigits x = sum x

validate :: Integer -> Bool

