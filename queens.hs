queens :: Int -> [[Int]]
queens 0  =  [[]]
queens n  |  n > 0
          =  [q:b | q <- [1..8], b <- queens (n-1),
                    safe q b]

safe :: Int -> [Int] -> Bool
safe q b  =  null [d | (d,qa) <- zip [1..] b,
                       q == qa || abs (q-qa) == d]

