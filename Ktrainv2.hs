import System.Environment
import Data.Time

timed :: IO a -> IO (a,Int)
timed action  =  do tstart <- getCurrentTime
                    x      <- action
                    tstop  <- getCurrentTime
                    let t   = floor $ diffUTCTime tstop tstart
                    return (x,t)

main :: IO ()
main = do [filename] <- getArgs
          flines <- readFile filename >>= return . lines
          ((w,e),t) <- timed (trainWith (0,0) flines)
          putStr ("speed " ++ show(w*60 `div` t) ++ "wpm, ")
          putStrLn ("accuracy " ++ show(((w-e) * 100) `div` w) ++ "%")

trainWith :: (Int, Int) -> [String] -> IO (Int, Int)
trainWith we [] = return we
trainWith (w, e) (x:xs) = do putStrLn x
                             l <- getLine
                             let (w',e') = check x l
                             let (w'',e'') = (w+w',e+e')
                             putStrLn (show w'' ++ " words, " ++ show e'' ++ " errors.")
                             trainWith (w'',e'') xs

check :: String -> String -> (Int, Int)
check a b = (length as, diffs as bs)
            where
            as = words a
            bs = words b

diffs :: [String] -> [String] -> Int
diffs [] bs = length bs
diffs as [] = length as
diffs (a:as) (b:bs) = if a == b then diffs as bs
                      else 1 + (diffs as bs)
