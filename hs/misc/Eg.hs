module Eg where


(|>) :: a -> (a -> b) -> b
x |> f = f x

(/>) :: Monad m => m a -> (m a -> m b) -> m b
x /> f = f x

demo1 x = x |> (+1)
            |> (+2)
            |> (*10)

start :: Int -> IO Int
start x = do
    putStrLn (show x)
    return x

add :: Int -> Int -> IO Int
add x y = do
    let res = x + y
    putStrLn (show res)
    return (res)

--demo2 x = return x
--        /> (add 10)
--        /> (add 20)