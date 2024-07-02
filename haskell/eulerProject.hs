import GHC.Real (Integral)
import GHC.Num (floatFromInteger)

eulerProject1 :: (Integral a) => a -> a -> a
eulerProject1 lower upper = sum [n | n <- [lower .. upper - 1],
                                     n `mod` 3 == 0 ||
                                     n `mod` 5 == 0]

eulerProject2 :: Integer
eulerProject2 = sum [n | n <- takeWhile (< 4000000) infLazyFibs, even n]
--eulerProject2 = sum[n | n <- takeWhile (< 4000000) (map fib [1..]), even n]

-- low perf ...
eulerProject3 :: Integer -> Integer
eulerProject3 n
    | n <= 0 = error "input must be > 0 !!!"
    | n == 1 = 1
    | otherwise = last (filter isPrime (factors n))

eulerProject4 :: Int -> Int
eulerProject4 n = maximum [z | x <- [1..n], y <- [1..n], let z = x * y, isPalindromNum z]

eulerProject4' :: Int -> Int
eulerProject4' n = maximum [z | x <- [1..n], y <- [x..n], let z = x * y, isPalindromNum z]

eulerProject5 :: [Integer] -> Integer
eulerProject5 [] = 0
eulerProject5 xs = head [k | k <- [1..], allBool(evenlyDivisable k xs)]



{----- Utils -----}
evenlyDivisable :: Integer -> [Integer] -> [Bool]
evenlyDivisable _ [] = []
evenlyDivisable n xs = [n `mod` k == 0 | k <- xs]

allBool :: [Bool] -> Bool
allBool [] = False
allBool xs = foldr (\t acc -> acc && t) True xs

revNumList :: (Num a, Ord a) => a -> a -> [a]
revNumList b a
    | a == b = [a]
    | a > b = []
    | otherwise = b : revNumList (b - 1) a

isPrime :: Integer -> Bool
isPrime n
    | n <= 0 = False
    | n == 1 = False
    | otherwise = null [x | x <- [2..n-1], n `mod` x == 0]


isPalindromNum :: Int -> Bool 
isPalindromNum s = show s == reverse (show s)

generateCombinations :: Int -> Int -> [Int] 
generateCombinations a b = [a * b | a <- [1 ..a], b <- [1 .. b]]

fib :: Int -> Integer
fib n = fibs!!n where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs :: Int -> [Integer]
fibs n = [fib n | n <- [2 .. n + 1]]

infLazyFibs :: [Integer]
infLazyFibs = xs where xs = 1 : 1 : zipWith(+) xs (tail xs)

lazyFibs :: Int -> [Integer]
lazyFibs n = take n infLazyFibs

factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod` x == 0]
