module Shape(Shape, getX, getY, setX, setY, moveTo, rMoveTo, draw) where

-- declare method interfaces for the shape superclass
class Shape a where
    getX :: a -> Int
    getY :: a -> Int
    setX :: a -> Int -> a
    setY :: a -> Int -> a
    moveTo :: a -> Int -> Int -> a
    rMoveTo :: a -> Int -> Int -> a
    draw :: a -> IO()



