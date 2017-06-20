module TypeClass where
    
class Contract a where
    arrange :: a -> Bool

instance Contract [a] where
    arrange [] = False
    arrange _ = True

instance Contract Int where
    arrange 0 = False
    arrange _ = True

instance Contract Bool where
    arrange = id

instance Contract (Maybe a) where
    arrange (Just _) = True
    arrange Nothing = False  

main = do
    putStrLn "Hello World"