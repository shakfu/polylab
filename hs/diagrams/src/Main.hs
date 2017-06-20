{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


eg1 :: Diagram B
eg1 = circle 1

eg2 :: Diagram B
eg2 = circle 1  # fc blue
                # lw veryThick
                # lc purple
                # dashingG [0.2, 0.05] 0


mkstar :: Int -> Int -> Diagram B
mkstar p s = star (StarSkip s) (regPoly p 1) # strokeP 

eg3 :: Diagram B
eg3 = star (StarSkip 2) (regPoly 8 1) # strokeP  

eg4 :: Diagram B
eg4 = star (StarSkip 3) (regPoly 8 1) # strokeP

eg5 :: Diagram B
eg5 = star (StarSkip 4) (regPoly 8 1) # strokeP

eg6 :: Diagram B
eg6 = star (StarSkip 5) (regPoly 8 1) # strokeP



-- layer diagrams on top of each other with color
example1 :: Diagram B
example1 = mconcat [ eg6 -- # fc pink # opacity 0.3
                   , eg5 -- # fc yellow # opacity 0.3
                   , eg4 -- # fc lightblue
                   , eg3 -- # fc red # opacity 0.3
                   ]

example :: Diagram B
-- example = mconcat $ map (mkstar 8) [2,3,4,5,6]
example = mconcat [mkstar p s | p <- [7,8], s <- [2 .. 5]]




main = mainWith example


