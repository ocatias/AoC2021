import Data.List.Split
import Data.List

-- Ensures that the tuple with the smaller value is on the left (if horizontal / vertical), otw. the smaller x value is on the left
simplify [[x1, y1], [x2, y2]]
    | x1 == x2 = if y1 <= y2 then [x1, y1, x2, y2] else [x1, y2, x2, y1]
    | y1 == y2 = if x1 <= x2 then [x1, y1, x2, y2] else [x2, y1, x1, y2]
    | otherwise =  if x1 <= x2 then [x1, y1, x2, y2] else [x2, y2, x1, y1]

-- Only vertical and horizontal lines
compute_lines [x1, y1, x2, y2]
    | x1 == x2 = [(x1, y) | y <- [y1 .. y2]]
    | y1 == y2 = [(x, y1) | x <- [x1 .. x2]]
    | otherwise = []

-- Also horizontal
compute_lines2 [x1, y1, x2, y2]
    | (x1 /= x2) && (y1 < y2) = [(x1 + d, y1 + d) | d <- [0 .. x2-x1]]
    | (x1 /= x2) && (y1 > y2) = [(x1 + d, y1 - d) | d <- [0 .. x2-x1]]
    | otherwise = compute_lines [x1, y1, x2, y2]


combine ((x:xs):ys) = [length (x:xs)] ++ (combine ys)
combine _ = []

-- Returns a list of tuples (nr_of_occurences_of, item)
count  = combine . group . sort

-- Combine different points into list, count how often each unique element appears, remove all number of singular appearences, count nr of appearances left
lines_to_nr_intersections = length . filter (\x -> x > 1) . count . concat

main = do 
    content <- readFile ("input.txt")
    let inputAsStrings = lines content
    let input1 = map (splitOn " -> ") inputAsStrings
    let input2 = map (map (splitOn ",")) input1
    let input3 = map (map (map ((\x -> read x)::(String -> Int)))) input2
    let input = map (\x -> simplify x) input3

    let nr_part1 = lines_to_nr_intersections $ map compute_lines input
    let nr_part2 = lines_to_nr_intersections $ map compute_lines2 input

    putStrLn $ show $ nr_part1  
    putStrLn $ show $ nr_part2