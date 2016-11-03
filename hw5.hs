-- Program: hw5.hs
-- Authors: Brandon Rullamas and Christina Duran
-- On this homework, we worked together for 2 hours,
-- Brandon worked independently for 2 hours,
-- and Christina worked independently for 3 hours.
-- CMPS 112
-- Flanagan
-- HW5

import Data.Char

import Control.Monad
import Control.Applicative (Applicative,pure,(<*>))

-- Parser Framework

data Parser a = Parser (String -> [(a,String)])

run :: Parser a -> String -> [(a,String)]
run (Parser f) s = f s

satP :: (Char -> Bool) -> Parser Char
satP pred = Parser (\cs -> case cs of
                            []    -> []
                            c:cs' -> if pred c then [(c,cs')] else [])

digit = satP isDigit

instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)])
   pa >>= fpb = Parser (\cs -> do (a,cs') <- run pa cs 
                                  run (fpb a) cs')

instance Functor Parser where
   fmap = liftM

instance Applicative Parser where
   pure = return
   (<*>) = ap

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser (\cs -> run p2 cs ++ run p1 cs)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = return [] <|> oneOrMore p

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p
                 xs <- zeroOrMore p
                 return (x:xs)

first :: Parser a -> Parser a
first p = Parser (\cs -> case run p cs of
                          [] -> []
                          (r:rs) -> [r])

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p opp = do x <- p
                 tryMore x
   where tryMore x = first (return x <|> more x)
         more x = do op <- opp
                     y <- p
                     tryMore (op x y)

-- Calculator

-- intP :: Parser Int
-- intP = do digits <- first (oneOrMore digit)
--          return (read digits)

-- addOp :: Parser (Int -> Int -> Int)
-- addOp = do satP (== '+') ; return (+)
--    <|> do satP (== '-') ; return (-)

-- mulOp :: Parser (Int -> Int -> Int)
-- mulOp = do satP (== '*') ; return (*)
--    <|> do satP (== '/') ; return div

-- calc :: Parser Int
-- calc = let mulExpr = chain intP mulOp
--       in chain mulExpr addOp

-- Problem 4 Main Function
doubleP :: Parser Double
doubleP = first doubleHelp

-- Problem 4 Helper Function
doubleHelp :: Parser Double
doubleHelp = do digits <- first (oneOrMore digit)
                return (read digits)
         <|> do dig <- (first (oneOrMore digit))
                (satP(== '.'))
                d <- (first (zeroOrMore (digit)))
                return (read (dig ++ "." ++ d))

-- Problem 4 Helper Function
addOp :: Parser (Double -> Double -> Double)
addOp = do satP (== '+') ; return (+)
    <|> do satP (== '-') ; return (-)

-- Problem 4 Helper Function
mulOp :: Parser (Double -> Double -> Double)
mulOp = do satP (== '*') ; return (*)
    <|> do satP (== '/') ; return (/)

-- Problem 4 Main Function
calc :: Parser Double
calc = let mulExpr = chain doubleP mulOp
       in chain mulExpr addOp

-- Problem 5 Helper Function
expOp :: Parser (Double -> Double -> Double)
expOp = do satP (== '^') ; return (**)

-- Problem 5 Main Function
calc2 :: Parser Double
calc2 = let mulExpr = chain doubleP expOp
        in chain (chain mulExpr mulOp) addOp

-- Problem 6 Helper Function
parensP :: Parser Double
parensP = do satP(== '(')
             calc <- calc3
             satP(== ')')
             return calc

-- Problem 6 Main Function
calc3 :: Parser Double
calc3 = let mulExpr = doubleP <|> parensP
        in chain (chain (chain mulExpr expOp) mulOp) addOp
