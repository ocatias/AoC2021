import Data.List.Split
import Data.List

combine ((x:xs):ys) = [(x, length (x:xs))] ++ (combine ys)
combine _ = []
count  = combine . group . sort

distance x c = abs ((fst c) - x)

cost crabs x =  sum [ snd c * (distance x c) | c <- crabs]
cost2 crabs x =  sum [ fct c | c <- crabs]
    where 
        fct c = ((distance x c) + 1) * (distance x c) * (snd c)  `div` 2

main = do 
    rawInput <- readFile ("input.txt")
    let crabs = count $ map ((\x -> read x)::(String -> Int)) $ splitOn ","  $ (splitOn "\n" rawInput)!!0
    let max_x = maximum $ fst $ unzip crabs
    putStrLn $ show $ minimum [cost crabs x | x <- [0..max_x]]
    putStrLn $ show $ minimum [cost2 crabs x | x <- [0..max_x]]