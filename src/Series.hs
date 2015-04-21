-- | This module implements the functions for the series. 
module Series
 (
   -- * Part 1 of the exercise:
   first, grow, series, seriesN,
   -- * Part 2 of the exercise:
   special1, special2
 ) 
 where
import Data.List(sort)

-- To keep track of duplicate values
import qualified Data.Set as Set

-- I import (~==) to enable use of approximate equality.
import Data.AEq(AEq, (~==))

-- |The first number accepts a parameter x and calculates the 
-- function ((0.5 * x^2) + (30 * x) + 10) / 25
first :: Double -> Double
first x = (0.5 * x ** 2 + 30 * x + 10) / 25

-- |The growth rate for the series takes a parameter y and the first number 
-- with the parameter x already applied.
grow :: Double -> Double -> Double
grow fstNum y = (0.02 * y) / 25 / fstNum

-- |The overall series accepts two parameters 
-- fstNum - the first number with the parameter x already applied
-- rate - the growthrate from grow (with both parameters already applied)
--
-- If the current number in the series is approximately equal to the next number,
-- we end the series.
-- 
-- I deliberately do NOT include a length parameter, since this would result
-- in a list that after rounding, sorting and removing duplicates, does 
-- not have the required length. Instead we return a possibly infinite list.
series :: Double -> Double -> [Double]
series fstNum rate = fstNum : series' 1 fstNum
  where 
    series' index current 
      | current ~== next = []
      | isInfinite next || isNaN next = [] 
      | otherwise        = next : series' (index + 1) next  
      where next = rate * (fstNum ** index) 

-- |Takes n elements of a list, without taking any duplicates. 
takeLenNub :: (Ord a, AEq a) => Int -> [a] -> [a]
takeLenNub m zs = takeLenNub' m zs Set.empty
  where 
    takeLenNub' n _        _ 
      | n <= 0                = []
    takeLenNub' _ [x]      s 
      | Set.member x s        = []
      | otherwise             = [x]
    takeLenNub' n (x : xs) s 
      | Set.member x s        = takeLenNub' n xs s
      | otherwise             = x : takeLenNub' (n - 1) xs (Set.insert x s)
    takeLenNub' _ _        _  = []    


-- |The series is calculated using a function that accepts three parameters
-- fstNum - the first number with the parameter x already applied
-- rate - the growthrate from grow (with both parameters already applied)
-- len - the length of the resulting series
--
-- Duplicates are removed AFTER rounding.
seriesN :: Double -> Double -> Int -> [Double]
seriesN fstNum rate len = sort . takeLenNub len . map roundQuarter 
                               $ series fstNum rate

-- |Round a double to its nearest quarter
-- We check whether rounding the number does not round it to infinity.
-- If it does we keep the original number.
roundQuarter :: Double -> Double
roundQuarter d | isInfinite roundedD ||
                 isNaN      roundedD    = d
               | otherwise              = roundedD
  where roundedD = (fromInteger . round $ d * 4.0) / 4.0

-- |Takes the third largest number from a series
special1 :: [a] -> Either String a
special1 l@(_:_:_:_) = Right . last . init . init $ l
special1 _           = Left "--. Trying to take a special1 number of a series that is less than length 3"

-- |Assuming as input, a sorted series, we can go through the 
-- series linearly to find the closest match.
-- 
-- If 0 is chosen as the divisor, it will try to find the number closest
-- to infinity. (This could be changed to return Left <errormsg> instead.)
special2 :: Double -> Double -> [Double] -> Either String Double
special2 y z (d : ds) = special2' d ds
  where 
    approxN = y / z
    special2' m []             = Right m
    special2' m (e : es) 
       | abs (approxN - e) < 
         abs (approxN - m)     = special2' e es
       | abs (approxN - e) == 
         abs (approxN - m)     = special2' (max e m) es
       | otherwise             = special2' m es
special2 _ _ _ = Left "__. Series is empty! This should never occur!"