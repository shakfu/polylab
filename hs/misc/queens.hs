import System.Environment

type Queen = (Int,Int)

    
solveQueens :: Int -> Maybe [Queen]
solveQueens n
    | n < 2 = Nothing
    | otherwise = solveQueens' n []


-- Modified to take advantage of backtracking
solveQueens' :: Int -> [Queen] -> Maybe [Queen]
solveQueens' n stack
    | x < 1 = Just stack
    | otherwise = foldl loop Nothing [1..n]
    where x = n - length stack
          loop (Just s) y = Just s
          loop Nothing  y =
              if isValid stack (x,y)
                  then solveQueens' n $ (x,y):stack
                  else Nothing
                  
-- ~ oldsolveQueens' :: Int -> [Queen] -> Maybe [Queen]
-- ~ oldsolveQueens' n stack
-- ~     | x < 1 = if isValid stack then Just stack else Nothing
-- ~     | otherwise = foldl loop Nothing [1..n]
-- ~     where x = n - length stack
-- ~           loop (Just s) y = Just s
-- ~           loop Nothing  y = solveQueens' n $ (x,y):stack

-- Modified to take advantage of backtracking
isValid :: [Queen] -> Queen -> Bool
isValid stack (x2,y2) = foldl validate True stack
    where validate False _ = False
          validate True (x1,y1) =
              x1 /= x2 && y1 /= y2 && (abs $ y2 - y1) /= (abs $ x2 - x1)
              
-- ~ oldisValid :: [Queen] -> Bool
-- ~ oldisValid stack = foldl validate True pairs
-- ~     where pairs = [(s1,s2) | s1 <- stack, s2 <- stack, s1 /= s2]
-- ~           validate False _ = False
-- ~           validate True ((x1,y1), (x2,y2)) =
-- ~               x1 /= x2 && y1 /= y2 && (abs $ y2 - y1) /= (abs $ x2 - x1)

main :: IO ()
main = do
    args <- getArgs
    let n = case args of [] -> 8
                         (a:rgs) -> read a :: Int
    putStrLn $ show (solveQueens n)
