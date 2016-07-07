-- | This module is a generic implementation
--   of the ID3 decision tree algorithm.
--
-- A choice node on a ``continuous'' attribute is
-- handled by splitting the population in two via the mean attribute value.

module ID3 where

import Data.Ord
import Data.List
import qualified Data.Map as Map

data DecisionTree item outcome = Choice (item -> DecisionTree item outcome)
                               | Leaf outcome

data Attribute item = Discrete (item -> Integer)
                    | Continuous (item -> Double)


runDecisionTree :: DecisionTree item outcome -> item -> outcome
runDecisionTree (Leaf outcome) _ = outcome
runDecisionTree (Choice f) item = runDecisionTree (f item) item

id3 :: (Ord outcome) => [Attribute item] -> [(item, outcome)] -> DecisionTree item outcome
-- When there are no unused attributes left, select the most common outcome.
id3 [] xs = Leaf $ fst $ head $ sortBy (comparing (negate.snd)) $ histogram (map snd xs)
-- When all the items have the same outcome, pick that outcome
id3 attrs xs | allEqual (map snd xs) = Leaf $ snd $ head xs
-- Otherwise pick the attribute with minimum entropy
             | otherwise =
    let (bestAttr:moreAttrs) = sortBy (comparing (informationGain xs)) attrs in
    case bestAttr of
         Discrete attr ->
             let attrTreeMap = Map.fromList attrTrees
                 allAttrValues = nub $ map (attr . fst) xs
                 subtree v = id3 moreAttrs (filter (\(x,_) -> v /= attr x) xs)
                 attrTrees = [(v, subtree v) | v <- allAttrValues]
             in Choice $ \item -> case Map.lookup (attr item) attrTreeMap of
                                       Just subtree -> subtree
                                       Nothing -> error "id3: encountered a discrete attribute value that wasn't in the training set"
         Continuous attr ->
             let meanv = mean (map (attr.fst) xs)
                 ltTree = id3 moreAttrs (filter (\(x,_) -> attr x <  meanv) xs)
                 gtTree = id3 moreAttrs (filter (\(x,_) -> attr x >= meanv) xs)
             in Choice $ \item -> if attr item < meanv
                                     then ltTree
                                     else gtTree

informationGain :: Ord outcome => [(item, outcome)] -> Attribute item -> Double
informationGain xs (Discrete attr) =
    currentEntropy - sum (map term allAttributeValues)
    where
    currentEntropy = entropy (map snd xs)
    term a = probabilityOf (==a) * entropy (outcomesFor (==a))
    probabilityOf f = fromIntegral (length (outcomesFor f)) / fromIntegral (length xs)
    outcomesFor f = map snd $ filter (f . attr . fst) xs
    allAttributeValues = nub $ map (attr . fst) xs
informationGain xs (Continuous attr) =
    currentEntropy - term (< meanv) - term (>= meanv)
    where
    currentEntropy = entropy (map snd xs)
    term f = probabilityOf f * entropy (outcomesFor f)
    probabilityOf f = fromIntegral (length (outcomesFor f)) / fromIntegral (length xs)
    outcomesFor f = map snd $ filter (f . attr . fst) xs
    meanv = mean (map (attr.fst) xs)

entropy :: Ord a => [a] -> Double
entropy xs = sum $ map (\(_,n) -> term (fromIntegral n)) $ histogram xs
    where term 0 = 0
          term n = - (n / num) * log (n / num) / log 2
          num = fromIntegral (length xs)

histogram :: Ord a => [a] -> [(a, Int)]
histogram = buildHistogram Map.empty
    where buildHistogram map [] = Map.assocs map
          buildHistogram map (x:xs) = buildHistogram (Map.insertWith (+) x 1 map) xs

-- Simple "utility" functions
allEqual :: Eq a => [a] -> Bool
allEqual = and . mapAdjacent (==)

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs (tail xs)

mean :: (Real a, Fractional n) => [a] -> n
mean xs = realToFrac (sum xs) / realToFrac (length xs)


--------------------------------------------------------------------
-- Testing Area
--------------------------------------------------------------------
data Item = Item String Double Double Bool deriving Show

outlook (Item "sunny" _ _ _) = 1
outlook (Item "overcast" _ _ _) = 2
outlook (Item "rain" _ _ _) = 3

temp (Item _ i _ _) = (realToFrac i) / (realToFrac 100)

humidity (Item _ _ i _) = (realToFrac i) / (realToFrac 100)

windy (Item _ _ _ False) = 0
windy (Item _ _ _ True)  = 1

-- attributes
a1 = Discrete outlook
a2 = Continuous temp
a3 = Continuous humidity
a4 = Discrete windy

outlookData  = ["sunny","sunny","overcast","rain","rain","rain","overcast","sunny","sunny","rain","sunny","overcast","overcast","rain"]
tempData     = [85, 80, 83, 70, 68, 65, 64, 72, 69, 75, 75, 72, 81, 71]
humidityData = [85, 90, 78, 96, 80, 70, 65, 95, 70, 80, 70, 90, 75, 80]
windyData    = [False, True, False, False, False, True, True, False, False, False, True, True, False, True]
outcomes     = [0,0,1,1,1,0,1,0,1,1,1,1,1,0]

accuracy t = naccurate / ntotal
    where
    naccurate = fromIntegral $ length $ filter (== True) check
    ntotal = fromIntegral $ length check
    check = map (\x -> fst x == snd x) comparison
    comparison = zip outcomes $ map (runDecisionTree t) items

items = zipWith4 Item outlookData tempData humidityData windyData
d = zip items outcomes

t1 = id3 [a1] d
t2 = id3 [a2] d
t3 = id3 [a3] d
t4 = id3 [a4] d

t5 = id3 [a1,a2,a3,a4] d

results = map accuracy [t1, t2, t3, t4, t5]

----------------


