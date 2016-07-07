module Main where


bayes :: Double -> Double -> Double -> Double
bayes probBgivenNotA probBgivenA probA = (probBgivenA * probA) / probB 
    where
        probB = (probBgivenA * probA) + (probBgivenNotA * probNotA)
        probNotA = 1 - probA


probBgivenNotA = 0.00008
probBgivenA = 1
probA = 0.00005

scenario = bayes probBgivenNotA probBgivenA

-- main entrypoint
-- main = do
--    args <- getArgs
--    putStrLn $ "args: " ++ show args
--    if length args > 0 then
--        forM_ args $ \arg -> process arg
--        else process defaultDir
