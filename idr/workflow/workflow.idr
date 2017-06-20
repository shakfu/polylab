module Main

import Action.System

f : String -> String
f s = s ++ " Hello"


main : IO ()
main = do
  args <- getArgs
  traverse_ putStrLn args
  traverse_ putStrLn [getInfo, "bye"]
