import System.Environment
import Data.Time

-- TODO fix errorcount when user input too short
-- TODO add timing

errorCount :: Eq a => [(a,a)] -> Int
errorCount [] = 0
errorCount ((i,j):xs) = if i == j then
                        errorCount xs
                        else 1 + errorCount xs

main :: IO ()
main = do
        [n] <- getArgs
        l <- readFile n
        let linef [] w e = return ()
            linef (x:xs) w e = do   putStrLn x
                                    i <- getLine
                                    putStrLn (show cw ++ " words, " ++ show (e + (errorCount $ zip wx $ words i)) ++ " errors.")
                                    linef xs cw (e + (errorCount $ zip wx $ words i))
                                    where
                                        wx = words x
                                        cw = (length wx) + w
            in linef (lines l) 0 0
