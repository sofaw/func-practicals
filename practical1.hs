import Prelude hiding (take, drop, zipWith, showList)
import Data.List (intercalate, union)
import System.Environment

-- Q1 --
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) | n > 0
    = drop (n - 1) xs

-- Q2 --
positions2 :: Eq a => [a] -> a -> Int -> [Int]
positions2 [] i n = []
positions2 (x:xs) i n = if i == x
                            then n : positions2 xs i (n + 1)
                        else
                            positions2 xs i (n + 1) 

positions :: Eq a => [a] -> a -> [Int]
positions xs i = positions2 xs i 0

-- Q3 --
duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs) = if (length filtered == length xs)
                    then duplicates xs
                    else x : duplicates filtered
                    where filtered = filter (/=x) xs

-- Q4 --
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
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

myQuickSort :: Ord a => [a] -> [a]
myQuickSort [] = []
myQuickSort (x:xs) = let left = [y | y <- xs, y < x] in 
                        let right = [y | y <- xs, y >= x] in
                            (myQuickSort left) ++ x : (myQuickSort right)

-- Q5 --
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = (f x y) : zipWith f xs ys

-- Q6 --
data Mat a = Mat [[a]]
instance Show a => Show (Mat a) where
    show (Mat rows) = intercalate "\n" (map showLine rows)
        where
        showLine [] = ""
        showLine (x:xs) = (show x) ++ " " ++ showLine xs


transpose :: Mat a -> Mat a
transpose (Mat rows) = Mat (transposeRows rows)
                        where
                        transposeRows ([]:_)  = []
                        transposeRows rows = map head rows : transposeRows (map tail rows)

-- Q7 --
data Tri a = Tri [[a]]
instance Show a => Show (Tri a) where
    show (Tri []) = " "
    show (Tri (x:xs)) = (showLine (length xs) x ++  (show (Tri xs)))
                        where
                        showLinw _ [] = ""
                        showLine n x = (intercalate "" $ replicate n " ") ++ (intercalate " " (map show x)) ++ "\n"

mapRows :: [[a]] -> [[a]]
mapRows [] = []
mapRows rows = (mapRows $ map tail $ tail rows) ++ [map head rows]

trol :: Tri a -> Tri a
trol (Tri rows) = Tri (mapRows rows)

tror :: Tri a -> Tri a
tror (Tri rows) = Tri (map reverse $ mapRows rows)

-- Q8 --
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = let s = sublists xs in
                    s ++ map (x:) s
-- Q9 --
prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = [x] : map (x:) (prefixes xs)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (x:[]) = [x] : []
suffixes (x:xs) = let s = suffixes xs in
                    (x : (head s)) : s
-- Q10 --
sequences :: Ord a => [a] -> [[a]]
sequences (x:[]) = [x] : []
sequences (x:xs) = let s = sequences xs in
                    let c = filter (\s_elem -> head s_elem > x) s in
                    [x] : s ++ map (x:) c

segments :: Ord a => [a] -> [[a]]
segments (x:[]) = [x] : []
segments (x:y:ys) = let s = segments (y:ys) in
                     let c = filter (\e -> head e == y) s in
                    [x] : s ++ map (x:) c
                        

-- Q11 --
parts :: [a] -> [[[a]]]
parts (x:[]) = [[x]] : []
parts (x:xs) = let p = parts xs in
                map ([x]:) p ++ map (\y -> (([x]++) $ head y) : (tail y)) p

-- Q12 --
insertAt :: Int -> a -> [a] -> [a] 
insertAt z y xs = as ++ (y:bs)
                  where
                    (as,bs) = splitAt z xs

insertAllPos :: Int -> a -> [a] -> [[a]]
insertAllPos (-1) _ _ = []
insertAllPos n x ys = (insertAt n x ys) : insertAllPos (n-1) x ys

perms :: [a] -> [[a]]
perms (x:[]) = [x] : []
perms (x:xs) = let p = perms xs in
                let l = length (head p) in
                concat $ map (insertAllPos l x) p

-- Q13 --
change :: [Int] -> Int -> [[Int]]
change [] _ = []
change _ m | m <= 0 = []
change (c:cs) m | c==m = [c] : (change cs m)
change (c:cs) m = let usec = [c:l | l <- ((change (c:cs) (m-c)) `union` (change cs (m-c)))] in
                let discardc = [l | l <- (change cs (m))] in
                usec ++ discardc

