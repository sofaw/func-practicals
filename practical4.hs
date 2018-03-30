import Test.LeanCheck
import Test.LeanCheck.Utils

-- Q1 --
test_takeZero :: Bool
test_takeZero = length (take 0 [1..5]) == 0

-- Q2 --
prop_take :: Int -> [Word2] -> Bool
prop_take n xs = n > 0 ==> length (take n xs) == min n (length xs)

prop_drop :: Int -> [Word2] -> Bool
prop_drop n xs = n > 0 ==> length (drop n xs) == max n (length xs)
