-- Program: hw1.hs
-- Authors: Brandon Rullamas and Christina Duran
-- On this homework, we worked together for 2 hours,
-- Brandon worked independently for 4 hours,
-- and Christina worked independently for 2 hours.
-- brullama and crduran
-- Flanagan

txt :: String
txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
    "get to their goal, and in the end, the thing they want the most ends " ++
    "up destroying them. In case of [2] this is a whale..."

-- Problem 1
citeAuthor :: String -> String -> String
citeAuthor first last = last ++ ", " ++ first

-- Problem 2
initials :: String -> String -> String
initials (first:_) (last:_) = first : "." ++ last : "."

-- Problem 3
title :: (String, String, Int) -> String
title (_, title, _) = title

-- Problem 4
citeBook :: (String, String, Int) -> String
citeBook (author, title, year) = title ++ " (" ++ author ++ ", " ++ (show year) ++ ")"

-- Problem 5
bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [book]= citeBook book
bibliography_rec (book:books) = citeBook book ++ "\n" ++ bibliography_rec books

-- Problem 6 helper function
averageHelper :: (String, String, Int) -> Int
averageHelper (_, _, year) = year

-- Problem 6
averageYear :: [(String, String, Int)] -> Int
averageYear bookList = div (sum (map averageHelper bookList)) (length bookList)

-- Problem 7
isReference :: String -> Bool
isReference ('[':_:"]") = True
isReference _ = False

references :: String -> Int
references ref = length (filter (isReference) (words ref))

-- Problem 8
getNum :: String -> Int
getNum ref = read (init (tail ref))

citeText :: [(String, String, Int)] -> String -> String
citeText books textRef = unwords (map (citedRef books)  (words textRef))
    where citedRef :: [(String, String, Int)] ->  String -> String
          citedRef books refer
            | isReference refer = citeBook (books !! ((getNum refer) -1))
            | otherwise         = refer

