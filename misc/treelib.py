

class Node:
    def __init__(self, name, value, children=[]):
        self.name = name
        self.value = value
        self.children = children
    
    def __iter__(self):
        yield self.value
        for child in self.children:
            for child_of_child in iter(child):
                yield child_of_child

root = Node("goal", 1, [
            Node("a2", 4),
            Node("a3", 6, [
                Node("a4", 5),
                Node("a5", 4)
            ])
        ])

from functools import partial

def treefold(f, t):
    if not t.children: return t.value
    return f(t.name, t.value, 
        map(partial(treefold, f), t.children)
    )

def sumtree(name, value, xs):
    return value + sum(xs)

def count(name, value, xs):
    return 1 + sum(xs)

def depth(name, value, xs):
    return 1 + max(xs)


for f in [sumtree, depth, count]:
    print treefold(f, root)

print sum(root)



'''
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
'''

