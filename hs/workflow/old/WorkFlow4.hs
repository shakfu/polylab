module WorkflowLib where

class Node a where
    register :: a -> IO ()

instance Node Activity where
    register a = print (val a)
    

data Activity = Activity {val :: Bool} deriving Show

a1 = Activity {val=True}
