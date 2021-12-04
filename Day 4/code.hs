import Data.List.Split
import Data.List 

-- Gets the score if "moves" are played on "board" and only checks for HORIZONTAL winning moves
-- This assumes that the only the last move in moves is (possibly) winning
get_score moves board
    | isWinner = score
    | otherwise = -1
    where   
        isWinner = (length [horizontal | horizontal <- board, ((intersect horizontal moves) ==  horizontal)]) > 0
        score = sum [ x | horizontal <- board, x <- horizontal, not (elem x moves)] * (last moves)

-- Flips the board by 90° allowing su to only check for vertical winning moves
flipBoard board = [[(board!!x)!!y | x <- [0..4]] | y <- [0..4]]

-- Like get_score but also checks for vertical winners
play_board moves board = maximum [get_score moves board, get_score moves (flipBoard board)]


-- Gets the score of all winners, in the order that they win
find_winners (x:xs) prevMoves boards 
    | score >= 0 = [score] ++ future_winners
    | otherwise = future_winners
    where 
        (scores, winning_boards) = unzip [(sc, board) | board <- boards, let sc = play_board prevMoves board, sc >= 0]
        score = if scores == [] then -1 else maximum scores
        future_winners = find_winners xs (prevMoves ++ [x]) (boards \\ winning_boards)

find_winners [] prevMoves [] = []

find_winners [] prevMoves boards
    | score >= 0 = [score]
    | otherwise = []
    where score = maximum [play_board prevMoves board | board <- boards]


main = do 
    content <- readFile ("input.txt")
    let linesAsStrings = lines content
    let sections = splitOn [""] linesAsStrings

    -- Split of the moves and stores them as a list of Ints
    let moves = map ((\x -> read x)::(String -> Int)) (splitOn "," ((head sections)!!0))

    -- Split of boards
    let boards1 = (tail sections)
    -- Split by " "
    let boards2 = map (map (\x -> splitOn " " x)) boards1
    -- Remove entries of the form ""
    let boards3 = map (map (\list -> filter (\e -> e/= "") list)) boards2
    -- Transforms strings into Ints
    let boards = map (map (map ((\x -> read x)::(String -> Int)))) boards3
    
    let winning_scores = find_winners moves [] boards 
    putStrLn $ show $ winning_scores
    putStrLn $ show $ head winning_scores
    putStrLn $ show $ last winning_scores

