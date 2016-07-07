module Polymorph(main) where
    
import Shape
import Circle
import Rectangle

main =
  do
     -- handle the shapes polymorphically
     drawloop scribble1
     drawloop scribble2

     -- handle rectangle specific instance
     draw arectangle
     draw (Rectangle.setWidth arectangle 30)

  where
     -- create lists containing instances of each shape
     scribble1 = (MakeCircle 15 25 8):[]
     scribble2 = (MakeRectangle 10 20 5 6):[]

     -- create a rectangle instance
     arectangle = (MakeRectangle 0 0 15 15)

-- iterate through the list of shapes and draw
drawloop [] = return True
drawloop (x:xs) =
  do
     draw x
     draw shapeMoved
     drawloop xs
  where
     shapeMoved = (Shape.rMoveTo x 100 100)
