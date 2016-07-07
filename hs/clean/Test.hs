module Test where

import Data.List

data Flag = Version | Path String deriving (Show, Eq)

flags :: [Flag]
flags = [Version, Path "hello there"]

flags' = [Version]

getPath :: [Flag] -> String
getPath flags = extract [path | Path path <- flags]
    where 
        extract [] = "."
        extract xs = head xs

getPath2 :: [Flag] -> String
getPath2 = foldr (\x ys -> case x of
                            Path a -> a
                            _      -> ys) "."

findPath :: [Flag] -> Maybe Flag
findPath = find (\x -> case x of 
                        (Path _) -> True
                        _        -> False)



