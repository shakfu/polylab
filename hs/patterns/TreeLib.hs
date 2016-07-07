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

treeFold' :: c -> (a -> b -> [c] -> c) -> Tree a b -> c
treeFold' acc f EmptyTree = acc
treeFold' acc f (Node name value children) = 
    f name value (map (treeFold' acc f) children)

-- curry the accumulator
treeFold = treeFold' 0


sumTree :: String -> Double -> [Double] -> Double
sumTree name value xs = value + sum xs

count :: (Num c) => a -> b -> [c] -> c
count name value xs = 1 + sum xs

depth :: (Num c, Ord c) => a -> b -> [c] -> c
depth name value xs = 1 + maximum0 (xs)


check = [ treeFold count t
        , treeFold sumTree t
        , treeFold depth t
        ]
