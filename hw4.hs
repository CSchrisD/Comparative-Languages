-- Program: hw4.hs
-- Authors: Brandon Rullamas and Christina Duran
-- On this homework, we worked together for 3 hours,
-- Brandon worked independently for 4 hours,
-- and Christina worked independently for 4 hours.
-- CMPS 112
-- Flanagan


-- ****** README ******
-- For #2, you need to add an empty string to your test.
-- So, trying to do 'test False' would fail,but doing
-- 'test "" False' should work.

{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

import System.Random
import Data.Int

-- Problem 1
class (Show a) => Gen a where
   gen :: IO a

-- random IO
instance (Show a, Random a) => Gen a where
   gen = randomIO

-- random IO tuple
instance (Gen a, Gen b) => Gen (a,b) where
   gen = do x <- gen
            y <- gen
            return (x,y)

-- random IO list
instance (Gen a) => Gen [a] where
    gen = do number <- (randomRIO(0,10) :: IO Int)
             if(number /= 0)
             then sequence ([gen | _ <- [1..number]] :: Gen a => [IO a])
             else return []

-- Problem 2 Helper Function
class Testable a where
--   test :: String -> a -> (String, IO Bool)
     test :: String -> a -> IO (String, Bool)

-- Problem 2 Helper Function
instance Testable Bool where
   test a b = return (a, b)

-- Problem 2
instance (Gen a, Testable b) => Testable (a -> b) where
   test str t = do n <- gen
                   let f = t n
                   if (str == "" )
                   then (test (show n) (f))
                   else (test (str ++ " " ++ show n) f)

-- Problem 3 and Problem 4
quickCheck :: (Testable a) => Int -> a -> IO()
quickCheck n t = if (n == 0)
                 then return ()
                 else do (x, y) <- test "" t
                         if (y == False)
                         then do putStr ("Failing Inputs = "++ x ++"\n")
                                 return ()
                         else quickCheck (n - 1) t

-- Problem 5 and 6, Corrected versions
isort :: [Int8] -> [Int8]
isort [] = []
isort (x:xs) = insert (isort xs)
    where insert [] = [x]
          insert (h:t) | x > h = h:insert t
                       | x <= h = x:h:t

qsort :: [Int8] -> [Int8]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [a | a <- xs, a > x]

-- Problem 5
testSort :: ([Int8] -> [Int8]) -> [Int8] -> Bool
testSort sort lst = if(length lst == 1 || length lst == 0)
                    then True
                    else if (head (sort lst) <= head (tail (sort lst)))
                         then testSort sort (tail (sort lst))
                         else False
