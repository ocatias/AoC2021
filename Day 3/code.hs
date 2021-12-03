-- Get bit at position n in a list of bits
get_bit_at (x:_) 0 = x
get_bit_at (x:xs) n = get_bit_at xs (n-1)

-- Get most common bit in a list of bits
most_common_bit (1:xs) nr_ones nr_zeros = most_common_bit xs (nr_ones + 1) nr_zeros
most_common_bit (0:xs) nr_ones nr_zeros = most_common_bit xs nr_ones (nr_zeros+1)
most_common_bit [] nr_ones nr_zeros 
    | nr_ones >= nr_zeros = 1
    | otherwise = 0

-- Converts a binary into an integer
convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x * 2^(length xs) + (convert xs)

-- Get most common bit at position n in a list of lists of bits
get_mcb ls n = most_common_bit [get_bit_at l n | l <- ls] 0 0


get_part2 [x] _ _ = x 
get_part2 ls n bit_fct = get_part2 (filter (\x -> x!!n == (bit_fct bit)) ls) (n+1) bit_fct 
    where bit = most_common_bit [get_bit_at l n | l <- ls] 0 0

get_oxy xs = get_part2 xs 0 (\x -> x)
get_co2 xs = get_part2 xs 0 (\bit -> 1 - bit) 

ord '0' = 0
ord '1' = 1

main = do 
    content <- readFile ("input.txt")
    let linesAsStrings = lines content
    let linesAsInt = [map ord xs | xs <- linesAsStrings]
    let nr_bits = (length (linesAsInt !! 0))
    let mostCommonBits = [get_mcb linesAsInt i | i <- [0.. nr_bits - 1]]
    let leastCommonBits = [1-bit | bit <- mostCommonBits]
    -- Part 1
    putStrLn $ show  $ convert(leastCommonBits) * convert(mostCommonBits)
    -- Part 2
    putStrLn $ show $ convert(get_oxy linesAsInt) * convert(get_co2 linesAsInt)