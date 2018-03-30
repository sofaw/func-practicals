import Data.List (intercalate)

-- Tri --
data Tri a = Tri {rows ::[[a]]}
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

-- Sorting --
insert :: Ord a => a -> [a] -> [a]
insert x [] = x : []
insert x (y:ys) = if x < y then (x : y : ys)
                    else y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- Q1 --
pascal :: Tri Integer
pascal = Tri (pascal')
        where
        pascal' = [1] : map (1:) (map pascalSum pascal')

pascalSum :: [Integer] -> [Integer]
pascalSum (x:[]) = [1]
pascalSum (x:y:ys) = (x+y) : pascalSum (y:ys)

-- Q2 --
hamming :: [Integer]
hamming = hamming'
        where
        doubles = map (2*) (1:hamming)
        trebles = map (3*) (1:hamming)
        quintuples = map (5*) (1:hamming)
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys) = if x == y
                                then x : (merge xs ys)
                                else if x < y then
                                    x : (merge xs (y:ys)) 
                                    else y : (merge (x:xs) ys)
        hamming' = merge doubles (merge trebles quintuples)

-- Q3 --
primes :: [Integer]
primes = sieve [2..]
         where
         sieve (p:xs) = p : filter (noFactorsIn primes) xs
         noFactorsIn (p:ps) x = x < p*p || x `mod` p > 0 && noFactorsIn ps x 

-- Q4 --
-- Inefficient solution
--queens :: Int -> [[Int]]
--queens 0  =  [[]]
--queens n  |  n > 0
--          =  [q:b | q <- [1..8], b <- queens (n-1),
--                    safe q b]

--safe :: Int -> [Int] -> Bool
--safe q b  =  null [d | (d,qa) <- zip [1..] b, q == qa || abs (q-qa) == d]

queens :: Int -> [[Int]]
queens n = queens' [1..n] [] []
           where
           queens' [] _ _ = [[]]
           queens' ms dr df = [ q:b | (q,etc) <- picks ms, 
                                      q `notElem` df, 
                                      q `notElem` dr, 
                                      b <- queens' etc 
                                           [r+1| r <- q:dr, r <= n] 
                                           [f-1| f <- q:df, f >= 1]]

picks :: [a] -> [(a,[a])]
picks [] = []
picks (x:xs) = (x,xs) : [(x',x:xs') | (x',xs') <- picks xs]
