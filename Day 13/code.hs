import Data.List.Split
import Data.List
import Data.Tuple

fold (x,y) ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=':change_string)
    | x == change = []
    | x < change = [(x, y)]
    | otherwise = [(change - (x - change), y)]
    where change = (read change_string) :: Int

fold (x,y) ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'y':'=':change_string) = map swap $ fold (y, x) ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=':change_string)

keep_folding points (move:moves) = keep_folding (nub $ concat $ ( map (\point -> fold point move) points)) moves
keep_folding points [] = points

main = do 
    rawInput <- readFile ("input.txt")
    let input = splitOn [""] (init (splitOn "\n" rawInput))
    let moves = input!!1
    let points = map (\[x,y] -> (read x, read y)::(Int, Int)) $ map (\x -> splitOn "," x) $ input!!0
    putStrLn $ show $ length $ nub $ map (\point -> fold point (moves!!0)) points 
    let final_points = sort $ keep_folding points moves
    let (width, height) = (maximum $ map (\x -> fst x) final_points,  maximum $ map (\x -> snd x) final_points)
    writeFile "output.txt" (concat [[if (x,y) `elem` final_points then '#' else '.' | x <- [0..width]]++"\n"  | y <- [0..height]])