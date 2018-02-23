import Prelude hiding (take, drop, zipWith)

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) | n > 0
    = drop (n - 1) xs

positions2 :: Eq a => [a] -> a -> Int -> [Int]
positions2 [] i n = []
positions2 (x:xs) i n = if i == x
                            then n : positions2 xs i (n + 1)
                        else
                            positions2 xs i (n + 1) 

positions :: Eq a => [a] -> a -> [Int]
positions xs i = positions2 xs i 0

duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs) = if (length filtered == length xs)
                    then duplicates xs
                    else x : duplicates filtered
                    where filtered = filter (/=x) xs


sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:y:ys) = if x < y
                then x : sort (y:ys)
                else y : sort (x:ys)

bubbleSort2 :: Ord a => [a] -> Int -> [a]
bubbleSort2 xs 0 = xs
bubbleSort2 xs n = bubbleSort2 (sort xs) (n - 1)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = bubbleSort2 xs (length xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = x : []
insert x (y:ys) = if x < y then (x : y : ys)
                    else y : insert x ys

sort2 :: Ord a => [a] -> [a]
sort2 [] = []
sort2 (x:xs) = insert x (sort2 xs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = (f x y) : zipWith f xs ys

data Mat a = Mat [[a]]
