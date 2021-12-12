import Data.List.Split
import Data.List
import Data.Char

is_big_cave:: [Char] -> Bool
is_big_cave name = ord (name!!0) >= 65 && ord (name!!0) <= 90

get_neighbors adj_list node = map (\[x,y] -> if x == node then y else x) $ filter (\edge -> node `elem` edge) adj_list

compute_paths _ "end" path _ _ = [path]
compute_paths adj_list node path fct_prev_small_caves fct_neighbor
    | length neighbor_candidates == 0 = []
    | otherwise = concat [ compute_paths adj_list neighbor (neighbor:path) fct_prev_small_caves fct_neighbor | neighbor <- neighbor_candidates]
    where
        prev_small_caves = filter (\x -> not $ is_big_cave x) path
        neighbor_candidates = [ neighbor | neighbor <- get_neighbors adj_list node, (is_big_cave neighbor) || not (neighbor `elem` path) || (fct_prev_small_caves prev_small_caves), fct_neighbor neighbor]

main = do 
    rawInput <- readFile ("input.txt")
    let adj_list = map (\x -> splitOn "-" x) $ (init (splitOn "\n" rawInput))
    putStrLn $ show $ length$ compute_paths adj_list "start" ["start"] (\_ -> False) (\_ -> True)
    putStrLn $ show $ length $ compute_paths adj_list "start" ["start"] (\x -> length (nub x) == length x) (\x -> not (x == "start"))
