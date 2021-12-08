import Data.List.Split
import Data.List
import Data.Char

part1 :: [String] -> (Int, Int, Int, Int)
part1 xs = ((nr 2), (nr 4), (nr 3), (nr 7))
    where nr n = length [x | x <- xs, length x == n]
    
get_digit:: [(String, Int)] -> String -> Int
get_digit ls str = [i | (s,i) <- ls, length s == length str, length (intersect s str) == (length str)]!!0

og_dig = map sort ["deagbc", "ab", "dafgc", "dafbc", "efab", "cdfbe", "defgbc", "dab", "defagbc", "defabc"]

idx :: Char -> Int
idx x = (ord x) - 97

remap:: String -> (String -> String)
remap ls = \xs -> [ ls!!(idx x) | x <- xs]

permutate :: (Eq a) => [a] -> [[a]]
permutate [] = [[]]
permutate l = [a:x | a <- l, x <- (permutate $ filter (\x -> x /= a) l)]

part2 (digits, y) (x:xs)
    | intersect digits key == digits = sum [([1000,100,10,1]!!i)*(get_digit dict (y!!i)) | i <- [0..3]]
    | otherwise = part2 (digits, y) xs
    where 
        key = map sort (map (remap x) og_dig)  
        dict = zip key [0..9]

main = do 
    rawInput <- readFile ("input.txt")
    let input = map (map (filter (\x -> x/= ""))) $ (map (map (splitOn " ")) $ map (splitOn "|") $ init (splitOn "\n" rawInput))
 
    let p1 = sum [ a+b+c+d | (a,b,c,d) <- (map (\x -> part1 (x!!1)) input)]
    let permutations = permutate "abcdefgh"
    let example = zip $ input !! 0
    putStrLn $ show $ p1
    let values = [part2 (map sort  (x!!0), (x!!1)) permutations | x <- input]
    putStrLn $ show $ values
    putStrLn $ show $ sum values