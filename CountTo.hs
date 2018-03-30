import BreadthFirst
import System.Environment
import BatchedQ as Q

countTo :: (Num a, Show a) => Int -> [a]
countTo x = take x (breadthFirst (\n -> [(2*n)+1, 2*(n+1)]) 0)

countToQ :: (Num a, Show a) => Int -> [a]
countToQ x = take x (bfsBatchedQ (\n -> (n+1)) 0)

getIntArg :: IO Int
getIntArg = fmap (read . Prelude.head) getArgs

main :: IO ()
main = do n <- getIntArg
          print (countTo n)
