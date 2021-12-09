import Data.List.Split
import Data.List

neigh_values c x y = map (\(x,y) -> (c!!y)!!x) (neigh_coord c x y)
    

neigh_coord c x y = l ++ r ++ u ++ d
    where 
        l = if x > 0 then [(x-1, y)] else []
        r = if x < ((length (c!!0))-1) then [(x+1,y)] else []
        u = if y > 0 then [(x,y-1)] else []
        d = if y < ((length c) - 1) then [(x,y+1)] else []

basins c x y prev = [(x,y)] ++  (concat [basins c nx ny (prev ++ [(x,y)] ++ candidates) | (nx,ny) <- candidates, not ((nx,ny) `elem` prev)])
    where 
        ns = neigh_coord c x y
        candidates = [(nx, ny) | (nx, ny) <- ns, (c!!ny)!!nx >= (c!!y)!!x,  (c!!ny)!!nx < 9]

main = do 
    rawInput <- readFile ("input.txt")
    let input = map (\xs -> [(read [x]) :: Int | x <- (xs::[Char])]) $ init (splitOn "\n" rawInput)
    let width = length (input!!0)
    let height = length input
    let low_points = filter (\(x,y) -> minimum (neigh_values input x y) > ((input!!y)!!x)) [(x, y) | x <- [0..(width - 1)], y <- [0..(height -1)]]
    putStrLn $ show $ sum $ map (\(x,y) -> (input!!y)!!x + 1) $ low_points
    let basinsSize = reverse $ sort $ map length $ map nub $ map (\(x,y) -> basins input x y []) low_points
    putStrLn $ show $ basinsSize!!0 * basinsSize!!1 * basinsSize!!2


