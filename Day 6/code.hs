import Data.List.Split
import Data.List

combine ((x:xs):ys) = [(x, length (x:xs))] ++ (combine ys)
combine _ = []

-- Returns a list of tuples (nr_of_occurences_of, item)
count  = combine . group . sort

change (0, number) = ((6, number), number)
change (x, number) = ((x-1, number), 0)

simplify :: [(Int, Int)] ->  [(Int, Int)]
simplify = map (\x -> (fst (x!!0), sum (snd $ unzip x)))  . groupBy (\ a b -> fst a == fst b) . sort

pass_day (x:xs) (ys)
    | newPop == 0 = pass_day xs (pop:ys)
    | otherwise = pass_day xs ([(8, newPop),pop] ++ ys)
    where (pop, newPop) = change x

pass_day [] ys = simplify ys

pass_days pop 0 = pop 
pass_days pop nrDays = pass_days (pass_day pop []) (nrDays - 1) 

count_pop = sum . snd . unzip

main = do 
    rawInput <- readFile ("input.txt")
    let populationStart = count $ map ((\x -> read x)::(String -> Int)) $ splitOn ","  $ (splitOn "\n" rawInput)!!0
    let pop1 = count_pop $ pass_days populationStart 80
    let pop2 = count_pop $ pass_days populationStart 256
    putStrLn $ show $ pop1
    putStrLn $ show $ pop2