-- Program: hw2.hs
-- Authors: Brandon Rullamas and Christina Duran
-- On this homework, we worked together for 2 hours,
-- Brandon worked independently for 2 hours,
-- and Christina worked independently for 3 hours.
-- CMPS 112
-- Flanagan
-- HW2

-- Problem 1
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl op base [] = base
myFoldl op base (x:xs) = op (myFoldl op base xs) x

-- Problem 2
myReverse :: [a] -> [a]
myReverse xs = foldl (\x b -> b:x) [] xs

-- Problem 3
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr op base xs = foldl (\y x -> x `op` y) base xs

-- Problem 4
myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 op base xs = foldr (\x y -> y `op` x) base xs

-- Problem 5
isUpper :: Char -> Bool
isUpper x = x `elem` ['A'..'Z']

-- Problem 6
onlyCapitals1 :: String -> String
onlyCapitals1 xs = filter (isUpper) xs

-- Problem 7
onlyCapitals2 :: String -> String
onlyCapitals2 xs = [x | x <- xs, isUpper x]

-- Problem 8
onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (x:xs)
                  | isUpper x = x:(onlyCapitals3 xs)
                  | otherwise = onlyCapitals3 xs
-- Problem 9
divRemainder :: Int -> Int -> (Int, Int)
divRemainder x y = (x `div` y, x `mod` y)

-- Problem 10
digitSum :: Int -> Int
digitSum 0 = 0
digitSum x
         | x `mod` 10 > 0 = (x `mod` 10) + digitSum (x `div` 10)
         | otherwise = digitSum (x `div` 10)

-- Problem 11 Helper Function
-- This function defines all of the words for an integer in the one's place
ones :: [String]
ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

-- Problem 11 Helper Function
-- This function defines all of the words for an integer in the one's place if the
-- value in the ten's place is greater than 0
teens :: [String]
teens = ["", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

-- Problem 11 Helper Function
-- This function defines all of the words for an integer in the ten's place
tens :: [String]
tens = ["", "ten", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- Problem 11 Helper Function
-- This function associates a word with the amount of integers that follow the place of the integer.
-- For example, if we look at a place in the number and see that 3 integers follow our current integer,
-- we can call numPrefix with a '3' and see that the current word association is 'thousand'.
numPrefix :: [(Int, String)]
numPrefix = [(3, "thousand"), (6, "million"), (9, "billion"), (12, "trillion"), (15, "quadrillion"),
             (18, "quintillion"), (21, "sextillion"), (24, "septillion"), (27, "octillion"),
                         (30, "nonillion"), (33, "decillion"), (36, "undecillion"), (39, "duodecillion"),
                         (42, "tredicillion"), (45, "quattuordecillion"), (48, "quindecillion"), (51, "sexdecillion"),
                         (54, "septendecillion"), (57, "octodecillion"), (60, "novemdecillion"), (63, "vigintillion")]

-- Problem 11 Helper Function
-- This function is to convert maybe string to string
findExpo :: Int -> String
findExpo y = x where (Just x) = (lookup (y) numPrefix)

-- Problem 11 Helper Function
-- This function uses the same logic as the numPrefix function in the sense that it finds how many values
-- are left and then uses the lists above to find the appropriate word value for that place of the number
convertInt :: String -> String
convertInt [] = ""
convertInt (x:xs)
                | (length xs) == 0 = (ones !! read [x])
                | (length xs) == 1 && x `elem` ['1'..'9'] && xs == "0" = (tens !! read [x])
                | (length xs) == 1 && x == '1' && xs /= "0"  = (teens !! read [(head xs)])
                | (length xs) == 1 && x `elem` ['2'..'9'] = (tens !! read [x]) ++  " " ++ convertInt xs
                | (length xs) == 2 && x `elem` ['1'..'9'] && (head xs) == '0' = (ones !! read [x]) ++ " hundred " ++ convertInt xs
                | (length xs) == 2 && x /= '0' = (ones !! read [x]) ++ " hundred " ++ convertInt xs
                | ((length xs) `mod` 3 == 1) && x == '0' && length (filter (== '0') (take 1 xs)) == 1 = convertInt (drop 1 xs)
                | ((length xs) `mod` 3 == 2) && x == '0' && length (filter (== '0') (take 2 xs)) == 2 = convertInt (drop 2 xs)
                | (length xs) `mod` 3 == 0 = (ones !! read [x]) ++ " " ++ findExpo (length xs) ++ " " ++ convertInt xs
                | (length xs) `mod` 3 == 1 && x == '1' && ((head xs) == '0') = (tens !! read [x]) ++ " " ++ convertInt xs
                | (length xs) `mod` 3 == 1 && x == '1' = (teens !! read [(head xs)]) ++ " " ++ convertInt ("0" ++ (drop 1 xs))
                | (length xs) `mod` 3 == 1 && x `elem` ['2'..'9'] = (tens !! read [x]) ++ " " ++ convertInt xs
                | (length xs) `mod` 3 == 2 && x /= '0' = (ones !! read [x]) ++ " hundred " ++ (findExpo ((length xs) - 2)) ++ " " ++ convertInt xs
                | otherwise = convertInt xs

-- Problem 11
sayNum :: Integer -> String
sayNum x = convertInt (show x)

