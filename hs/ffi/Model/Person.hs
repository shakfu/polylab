module Model.Person where

-- Person.hs

data Person = Person 
    { name :: String
    , age :: Int 
    } deriving (Show)