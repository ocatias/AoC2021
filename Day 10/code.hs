import Data.List.Split
import Data.List

count xs targets = length (filter (\x -> x `elem` targets) xs)

open_chars = ['(', '[', '{', '<']
close_chars = [')', ']', '}', '>']

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

score2 prev_score (x:xs) = score2 (prev_score*5 + (toInt (elemIndex x close_chars)) + 1) xs
score2 prev_score [] = prev_score

part1 :: [Char] -> [Char] -> (Int, [Char])
part1 (x:xs) stack
    | x `elem` open_chars = part1 xs (x:stack)
    | (elemIndex x close_chars) == (elemIndex (head stack) open_chars) = part1 xs (tail stack)
    | otherwise = (score x, [])

part1 _ stack = (0, stack)

toInt (Just x) = x
toInt _ = 0

main = do 
    rawInput <- readFile ("input.txt")
    let input = (init (splitOn "\n" rawInput))::[[Char]]
    let (scores, stacks) = unzip $ map (\x -> (part1 x [])) input
    putStrLn $ show $ sum scores
    let incompl_stacks = [stacks!!i | i <- [0..(length input) - 1], scores!!i == 0]
    let completion_Strings = map (map (\x -> close_chars!!(toInt (elemIndex x open_chars))))  incompl_stacks 
    let part2_scores = sort $ map (score2 0) completion_Strings
    putStrLn $ show $ part2_scores !! (((length part2_scores) - 1) `div` 2)
