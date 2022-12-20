import System.IO 

loop = do 
    putStrLn "Ready to roll? y/n"
    x <- getLine
    if x == "y"
        then do
            content <- readFile "input.txt"
            let lineList = lines content
            let sum = 0
            let maxSum = 0
            putStrLn $ "The most Calories carried: " ++ show (count sum maxSum lineList)
            loop
        else do
            putStrLn "Bye"
            return ()

count :: Int -> Int -> [String] -> Int
count _ maxSum [] = maxSum
count sum maxSum (x:xs)
    | x == ""   = count 0 (max sum maxSum) xs
    | otherwise = count (sum + (read x :: Int)) maxSum xs

main = loop
