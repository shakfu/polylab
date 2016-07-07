module Test

where

data Tree a b = EmptyTree | Node a b [Tree a b] 
                deriving (Show, Read, Eq)  


t =  Node "goal" 1.0 [
        Node "a2" 0.5 [
            Node "a3" 3.0 [
                Node "a4" 1.0 [
                    Node "a5" 1.0 []
                    ]
                ]
            ],
        Node "b2" 0.5 [
            Node "b3.1" 2.0 [],
            Node "b3.2" 2.0 [
                Node "b4" 10.0 []
                ]
            ]
     ]

maximum0 [] = 0
maximum0 xs = maximum xs

sumTree :: (Num b) => Tree a b -> b
sumTree EmptyTree = 0
sumTree (Node _ value children) = value + sum (map sumTree children)

count :: Tree a b -> Int
count EmptyTree = 0
count (Node _ value children) = 1 + sum (map count children) 

depth :: Tree a b -> Int
depth EmptyTree = 0
depth (Node _ value children) = 1 + maximum0 (map depth children)

treeFold :: c -> (a -> b -> [c] -> c) -> Tree a b -> c
treeFold acc f EmptyTree = acc
treeFold acc f (Node name value children) = f name value (map (treeFold acc f) children)

--  foldr :: (a -> b -> b) -> b -> [a] -> b
--  foldr f z []     = z
--  foldr f z (x:xs) = f x (foldr f z xs)

treeFold' :: (a -> b -> [c] -> c) -> c -> Tree a b -> c
treeFold' f z EmptyTree = z
treeFold' f z (Node name value children) = f name value (map (treeFold' f z) children)

sumTree' :: String -> Double -> [Double] -> Double
sumTree' name value xs = value + sum xs

count' :: (Num c) => a -> b -> [c] -> c
count' name value xs = 1 + sum xs

depth' :: (Num c, Ord c) => a -> b -> [c] -> c
depth' name value xs = 1 + maximum0 (xs)


check = [ count t   == treeFold' count' 0 t
        , sumTree t == treeFold' sumTree' 0 t
        , depth t   == treeFold' depth' 0 t
        ]
