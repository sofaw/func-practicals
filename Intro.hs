import System.Environment

main :: IO ()
main = do
        [n] <- getArgs
        putStrLn ("Hello, "++ n ++"!")
        p <- getProgName
        putStrLn ("My name is "++ p ++".")
