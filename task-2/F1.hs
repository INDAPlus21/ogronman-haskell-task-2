module F1 where
import Data.Char

l = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811,514229, 832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049]

fib n = l !! n


vokal = ['a', 'e', 'i', 'o', 'u', 'y']
konsonant = ['b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'z']


rovarsprak :: String -> String
rovarsprak [] = []

rovarsprak (x:xs)
    | x `elem` vokal = x:rovarsprak xs
    | otherwise = x:'o':x: rovarsprak xs


karpsravor :: String -> String
karpsravor [] = []

karpsravor (x:xs)
    |x `elem` konsonant = x:karpsravor(drop 2 xs)
    | otherwise = x:karpsravor xs


convertString :: String -> String
convertString [] = []
convertString (x:xs)
    |x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" = x:convertString xs
    |otherwise = ' ': convertString xs


medellangd :: String -> Double
medellangd s =  fromIntegral(sum(map length(words(convertString s)))) / fromIntegral(length(words(convertString s)))


numberdos :: [a] -> [a]
numberdos [] = []
numberdos [a] = [a]
numberdos (x:y:xs) = x:numberdos xs



skyffla :: [a] -> [a]
skyffla [] = []
skyffla [a] = [a]
skyffla (x:xs) = numberdos(x:xs) ++ skyffla(numberdos(xs))

