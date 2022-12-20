import System.IO 

loop = do 
    putStrLn "Ready to roll? y/n"
    x <- getLine
    if x == "y"
        then do
            content <- readFile "input_day2.txt"
            let lineList = lines content
            let sum = 0
            putStrLn $ "The total score: " ++ show (countSum sum lineList)
            loop
        else do
            putStrLn "Bye"
            return ()

countSum :: Int -> [String] -> Int
countSum sum [] = sum
countSum sum (l:lines) = do
    let x = head l
    let y = last l
    let newSum = sum + (countRound x y)
    countSum newSum lines

countRound :: Char -> Char -> Int
countRound x y
    | x == 'A' && y == 'X'  = 3+0
    | x == 'B' && y == 'X'  = 1+0
    | x == 'C' && y == 'X'  = 2+0
    | x == 'A' && y == 'Y'  = 1+3
    | x == 'B' && y == 'Y'  = 2+3
    | x == 'C' && y == 'Y'  = 3+3
    | x == 'A' && y == 'Z'  = 2+6
    | x == 'B' && y == 'Z'  = 3+6
    | x == 'C' && y == 'Z'  = 1+6

main = loop
