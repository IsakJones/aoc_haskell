import System.IO 
import Text.Printf

main = do
    nums <- readFile "input.txt"
    let formatted = format nums
        one = increases formatted
        two = increases $ windowSums formatted
        ans = "The %s answer is %d\n"
    printf ans "first" one
    printf ans "second" two

format :: String -> [Int]
format = map (read::String->Int) . lines 

increases :: [Int] -> Int
increases = length . filter (>0) . changes

changes :: [Int] -> [Int]
changes [_] = []
changes (x:y:xs) = y-x : changes (y:xs)

windowSums :: [Int] -> [Int]
windowSums (_:_:[])   = []
windowSums (x:y:z:xs) = x+y+z : windowSums (y:z:xs)