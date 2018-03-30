import System.Environment

main :: IO ()
main = do src <- getContents
          [w] <- getArgs
          putStrLn w
          putStrLn src
