module Priority where

import Data.List (sort, sortBy, elemIndex)
import Data.Maybe (fromJust)
import Data.Time (toGregorian, fromGregorian, localDay,
                  getZonedTime, zonedTimeToLocalTime)
import Data.Time.Calendar (diffDays)
import Control.Monad (forM, mapM_)



type Workflow = String
type Weight = Double
type Value = 

weights :: [(Workflow, Weight)]
weights = [ ("Tender",       1.0),
            ("LeaveRequest", 0.1)
          ]

data Task = Task {
    priority   :: Double,
    kind       :: String,
    weight     :: Double,
    value      :: Double,
    due        :: (Integer, Int, Int),
    importance :: Double,
    urgency    :: Double

} deriving (Eq, Show)

records :: [(Workflow, Double, (Integer, Int, Int))]
records =
    [ ("Tender",       100.0,  (2009, 3, 1))
    , ("Tender",       1000.0, (2009, 4, 1))
    , ("Tender",       100.0,  (2009, 5, 1))
    , ("Tender",       100.0,  (2009, 6, 1))
    , ("LeaveRequest", 100.0,  (2009, 7, 1))
    , ("Tender",       100.0,  (2009, 8, 1))
    , ("Tender",       100.0,  (2009, 9, 1))
    , ("Tender",       100.0,  (2009, 10,1))
    ]

process :: [(String, Double, (Integer, Int, Int))] -> IO [Task]
process records = do
    let total = sum [value | (_, value, _) <- records]
    due_days' <- mapM daysTo [date | (_, _, date) <- records]
    let due_days = reverse $ sort due_days'
    tasks <- forM records $ \record -> do
        let (kind, value, due_date) = record
        days <- daysTo due_date
        let weight = fromJust $ lookup kind weights
            relative_importance = value / (total + value)
            relative_urgency = percentrank due_days days
            task = Task {
                priority = weight * relative_importance + relative_urgency,
                kind = kind,
                weight = weight,
                value = value,
                due = due_date,
                importance = relative_importance,
                urgency = relative_urgency
            }
        return [task]
    return (concat tasks)

daysTo :: (Integer, Int, Int) -> IO Integer
daysTo (year, month, day) = do
    now <- getZonedTime
    let (y, m, d) = toGregorian (localDay (zonedTimeToLocalTime now))
        current = fromGregorian y m d
        prior = fromGregorian year month day
    return (diffDays prior current)

percentrank :: (Fractional b, Eq a) => [a] -> a -> b
percentrank xs x = rank / (n-1)
    where
        n = fromIntegral (length xs)
        rank = fromIntegral (fromJust $ elemIndex x xs)

prioritize :: [Task] -> [Task]
prioritize tasks = reverse $ sortBy compare tasks
    where
        compare Task {priority = p1}
                Task {priority = p2}
            | p1 < p2 = LT
            | otherwise = GT

main = do
    tasks <- process records
    let sorted = prioritize tasks
    mapM_ (putStrLn . show) sorted
