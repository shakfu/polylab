import Control.Monad
import Data.List
import Data.Maybe

data Val x = A x | B x | C x | D x | E x
  deriving (Show)

answer inputs pipeline =  do
  as <- tails $ (replicate (length pipeline) Nothing) ++ (map Just (reverse inputs))
  let bs = zipWith fmap (reverse pipeline) as
      cs = [ x | Just x <- bs ]
  return (reverse cs)

test = mapM_ print $ answer [5,4,3,2,1] [A, B, C, D]
