import Prelude hiding (take, drop, zipWith, showList)
import Data.List (intercalate)

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
instance Show a => Show (Mat a) where
    show (Mat rows) = intercalate "\n" (map showLine rows)
        where
        showLine [] = ""
        showLine (x:xs) = (show x) ++ " " ++ showLine xs


myQuickSort :: Ord a => [a] -> [a]
myQuickSort [] = []
myQuickSort (x:xs) = let left = [y | y <- xs, y < x] in 
                        let right = [y | y <- xs, y >= x] in
                            (myQuickSort left) ++ x : (myQuickSort right)

transpose :: Mat a -> Mat a
transpose (Mat rows) = Mat (transposeRows rows)
                        where
                        transposeRows ([]:_)  = []
                        transposeRows rows = map head rows : transposeRows (map tail rows)

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

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = let s = sublists xs in
                    s ++ map (x:) s

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = [x] : map (x:) (prefixes xs)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (x:[]) = [x] : []
suffixes (x:xs) = let s = suffixes xs in
                    (x : (head s)) : s

-- TODO: this generates all increasing sequences (non-consecutive)
--segments :: Ord a => [a] -> [[a]]
--segments (x:[]) = [x] : []
--segments (x:xs) = let s = segments xs in
--                    let c = filter (\s_elem -> head s_elem > x) s in
--                    [x] : s ++ map (x:) c


parts :: [a] -> [[[a]]]
parts (x:[]) = [[x]] : []
parts (x:xs) = let p = parts xs in
                map ([x]:) p ++ map (\y -> (([x]++) $ head y) : (tail y)) p

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
