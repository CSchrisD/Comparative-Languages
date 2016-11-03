-- Program: hw3.hs
-- Authors: Brandon Rullamas and Christina Duran
-- On this homework, we worked together for 2 hours,
-- Brandon worked independently for 3 hours,
-- and Christina worked independently for 4 hours.
-- CMPS 112
-- Flanagan
-- HW3

import Data.List

-- Problem 1
data BST k v = Empty |
               Node k v (BST k v) (BST k v)

val :: BST k v -> Maybe v
val Empty = Nothing
val (Node _ v _ _) = Just v

-- Problem 2
size :: BST k v -> Int
size Empty = 0
size (Node _ _ l r) = 1 + size l + size r

-- Problem 3
ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins n g Empty = (Node n g Empty Empty)
ins n g (Node k v l r) =
                       if (k == n)
                       then (Node k g l r)
                       else if (n < k)
                          then Node k v (ins n g l) r
                          else Node k v l (ins n g r)  

-- Problem 4
instance (Show v) => Show (BST k v) where
    show Empty = ""
    show (Node k v l r) = "(" ++ (show l) ++ (show v) ++ (show r) ++ ")"

-- Problem 5 Type Declaration
data JSON = JStr String
    | JNum Double
    | JArr [JSON]
    | JObj [(String, JSON)]

-- Problem 5
instance Show JSON where
   show j = showJson j

-- Problem 5 helper show JArr          
showJarr::[JSON] -> String
showJarr [] = []
showJarr (j:js) 
                | length js > 0 = (showJson j) ++ "," ++ showJarr js
                | otherwise = (showJson j)

-- Problem 5 helper show JObj
showJObj::[(String, JSON)] -> String
showJObj [] = []
showJObj (j:js) 
                | length js > 0 = (show (fst j)) ++ ":" ++ (showJson (snd j)) ++ "," ++ showJObj js
                | otherwise = (show (fst j)) ++ ":" ++ (showJson (snd j))

-- Problem 5 show json things
showJson::JSON -> String
showJson (JStr s) = show s
showJson (JNum d) = show (d)
showJson (JArr js) = "[" ++ showJarr js ++ "]" 
showJson (JObj kvs) = "{" ++ showJObj kvs ++ "}"

-- Problem 6 Class Declaration
class Json a where
    toJson :: a -> JSON
    fromJson :: JSON -> a

-- Problem 6 Instance Declaration for toJson
instance Json Double where
    toJson = JNum
    fromJson (JNum x) = x

-- Problem 6 Instance Declaration for toJson
instance (Json a) => Json [a] where
    toJson xs = JArr (map (toJson) xs)
    fromJson (JArr xs) = map (fromJson) xs
