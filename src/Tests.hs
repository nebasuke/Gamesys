-- | This module implements QuickCheck tests to test the Series module.
module Tests where
import Test.QuickCheck
import Series

-- I import (~==) to enable use of approximate equality on test cases.
import Data.AEq(AEq, (~==))
import Data.List (sort, nub)


-- |The first and second examples given in the assignment pdf. This should return True.
example1and2 :: Bool
example1and2 = 
  let fstNum     = first 1
      growthRate = grow fstNum 5062.5
      ss         = series fstNum growthRate
      ssRounded  = seriesN fstNum growthRate 5
  in fstNum ~== 1.62 && growthRate ~== 2.5 &&
     take 5 ss ~== [1.62, 4.05, 6.561, 10.62882, 17.2186884] &&
     ssRounded ~== [1.5, 4, 6.5, 10.75, 17.25] &&
     special1 ssRounded ~== Right 6.5 &&
     special2 1000 160 ssRounded ~== Right 6.5
     
-- |The length of a series with n elements, is at most n elements.
-- (It can be less due to the series never generating new elements, 
-- such as 0.0 0.0 2.)
prop_lengthOfSeriesN :: Double -> Double -> Int -> Property
prop_lengthOfSeriesN fstNum growthRate len = len >= 0
                                               ==> (length (seriesN fstNum growthRate len) <= len)

-- |A list is sorted if all pairs of elements are.
sorted :: Ord a => [a] -> Bool
sorted []  = True
sorted [_] = True
sorted (x : y : xs) = x <= y && sorted (y : xs)

-- |Property that states that every series of N length should be sorted
prop_sorted :: Double -> Double -> Int -> Bool
prop_sorted fstNum growthRate len = sorted $ seriesN fstNum growthRate len

-- |Check whether something is part of a list using approximately equals.
aElem :: AEq a => a -> [a] -> Bool
aElem = any . (~==)

-- |A number is rounded to a quarter, when after removing the integral part
-- it is either 0.0, 0.25, 0.5 or 0.75. 
isQuarter :: (AEq a, RealFrac a) => [a] -> Bool
isQuarter
  = foldr
      (\ x ->
         (&&) ((x - fromIntegral (floor x :: Integer)) 
                `aElem` [0.0, 0.25, 0.5, 0.75]))
      True

-- |All numbers in an n-series should be rounded to quarters
prop_quarters :: Double -> Double -> Int -> Property
prop_quarters fstNum growthRate len = len >= 0 ==> isQuarter $ seriesN fstNum growthRate len

-- |Takes the third largest element of a list, without doing any safety checks.
third :: Ord a => [a] -> a
third = head. tail . tail . reverse . sort

-- |Checks that any series of length 3 has a third largest element and checks whether
-- the error checking version is then equivalent to the naive version.  
prop_isThirdLargest :: Double -> Double -> Int -> Property
prop_isThirdLargest fstNum growthRate len = 
  let ss = seriesN fstNum growthRate len
  in length ss >= 3 ==> 
   case special1 ss of 
        Left _  -> False -- We got an error message despite the list being length 3
        Right x -> x == third ss 

-- |A series of N numbers should be equal to the same series filtered for duplicates.
prop_isUnique :: Double -> Double -> Int -> Bool
prop_isUnique fstNum growthRate len = 
  let ss = seriesN fstNum growthRate len
  in ss == nub ss 

-- |Call QuickCheck with 5000 tests instead.
quickCheck5000 :: Testable prop => prop -> IO ()
quickCheck5000 = quickCheckWith stdArgs { maxSuccess = 5000 }

-- |Apply QuickCheck to all properties.
quickCheckAllTests :: IO ()
quickCheckAllTests = do 
                  quickCheck example1and2
                  mapM_ quickCheck5000 
                        [prop_lengthOfSeriesN,
                         prop_quarters,
                         prop_isThirdLargest
                         ]
                  quickCheck5000 prop_sorted
                  quickCheck5000 prop_isUnique