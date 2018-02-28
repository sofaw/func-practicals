import System.Environment

-- TODO
errorCount :: Eq a => [(a,a)] -> Int
errorCount [] = 0
errorCount ((i,j):xs) = if i == j then
                        errorCount xs
                        else 1 + errorCount xs

repl :: (String -> String) -> String -> String
repl eval = unlines . map eval . lines

main :: IO ()
main = do
        [n] <- getArgs
        l <- readFile n
        let linef [] = return ()
            linef (x:xs) = do   putStrLn x
                                i <- getLine
                                putStrLn ((show . length) wx ++ " words, " ++ show (errorCount $ zip wx (words i)) ++ " errors.")
                                linef xs
                                where
                                    wx = words x
            in linef (lines l)
