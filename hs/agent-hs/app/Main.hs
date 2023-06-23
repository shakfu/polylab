-- newsagent.hs
module Main (main) where

-- stdlib
import Control.Monad
import Data.Maybe
import Data.List
import System.Environment (getArgs)
import System.Console.GetOpt
-- external modules
import Control.Concurrent.ParallelIO (parallel_, stopGlobalPool)
--import qualified Control.Concurrent.ParallelIO.Local as Local

-- internal modules
import Model
import Utils (info, downloadURL, percentTrue)
import Config (missions)
import Analysis (parseFeedString, analyzeFeed)
import Views.Console (showMission, showTarget)
import Views.Html (renderMissionInHtml)

-- other
import Lib


----------------------------------------------------------------------
-- Functions which consume a target and return it
--
-- f :: Target -> IO Targets
----------------------------------------------------------------------
downloadTarget :: Target -> IO Target
downloadTarget target = do
    content <- downloadURL (targetURL target)
    return target {targetData = Just content}

prepareTarget :: Target -> IO Target
prepareTarget target = do
    let feed = parseFeedString (fromJust $ targetData target)
    return target {targetFeed = feed}

analyzeTarget :: Target -> IO Target
analyzeTarget target = do
    entries <- analyzeFeed target
    let rs = map entryRelevance entries
    let nRelevant = fromIntegral (length $ filter (> 0) rs)
    let percentRelevant = nRelevant /  fromIntegral (length rs)
    return target { targetEntries = Just entries
                  , targetRelevance = percentRelevant
                  }

----------------------------------------------------------------------
-- Functional pipelining and sequencing helpers
--
-- note: opted for functions first api to facilitate currying,
--       order is key: e.g. withTarget [download, prepare, analyze]
----------------------------------------------------------------------

-- processing targets with a functional pipeline
actionTarget :: [Target -> IO Target] -> Target -> IO Target
actionTarget [] t  = return t
actionTarget fs t  = foldM (\ t f -> f t) t fs

actionTargets :: [Target -> IO Target] -> [Target] -> IO [Target]
actionTargets fs = mapM (actionTarget fs)

-- sequencing of fire-and-forget functions
doTarget ::  [Target -> IO ()] -> Target -> IO ()
doTarget fs t = forM_ fs $ \f -> f t

doTargets :: [Target -> IO ()] -> [Target] ->IO ()
doTargets fs = mapM_ (doTarget fs)

----------------------------------------------------------------------
-- core application top-level functions
--
-- note:
----------------------------------------------------------------------
processTargets :: [Target] -> IO [Target]
processTargets = actionTargets [ downloadTarget
                               , prepareTarget
                               , analyzeTarget
                               ]

postprocessTargets :: [Target] -> IO ()
postprocessTargets = doTargets [showTarget]


startMission :: Mission -> IO Mission
startMission m = do
	results <- processTargets (missionTargets m)
	return m {missionTargets=results}

reportMission :: Mission -> IO ()
reportMission m = do
	showMission m
	postprocessTargets (missionTargets m)
	renderMissionInHtml m

start :: [Mission] -> IO ()
start missions =
    forM_ missions $ \m -> do
        mission <- startMission m
        reportMission mission

start_p :: [Mission] -> IO ()
start_p missions = do
    parallel_ actions
    stopGlobalPool
    where
        actions = map doMission missions
        doMission m = do
            mission <- startMission m
            reportMission mission

startMissionByName :: String -> IO ()
startMissionByName name = do
    let mission = fromJust $ find (\m -> missionCodeName m == name) missions
    start [mission]


----------------------------------------------------------------------
-- Command Line processing
--
-- note: need to add version of above functions to take into
--       account command line options
----------------------------------------------------------------------

header = "Usage: newsagent [options]"

-- option types
data Flag = Verbose | NoHtml | Help | List | Parallel | Name String
            deriving (Show, Eq)

defaultMission = Name . fromMaybe "tech"

-- commandline options
options :: [OptDescr Flag]
options = 
    [ Option "h" ["help"]     (NoArg Help)     "display help"
    , Option "v" ["verbose"]  (NoArg Verbose)  "show verbose output"
    , Option "l" ["list"]     (NoArg List)     "list available missions"
    , Option "n" ["nohtml"]   (NoArg NoHtml)   "skip html output"
    , Option "p" ["parallel"] (NoArg Parallel) "runs agents in parallel"
    , Option "m" ["mission"]  (OptArg defaultMission "MISSION") "set mission"   
    ]

-- primary command line processing function
processArgs :: [Flag] -> IO ()
processArgs flags = do
    when (Help    `elem` flags) $ putStrLn $ usageInfo header options
    when (Verbose `elem` flags) $ dump flags
    when (List    `elem` flags) $ mapM_ putStrLn [missionCodeName m | m <- missions]
    startMissionFromFlags flags
    where
        dump fs = putStrLn $ "options: " ++ show fs
        startMissionFromFlags fs = case fs of
            [Name s] -> do
                putStrLn $ "starting mission: " ++ s
                startMissionByName s
            [Parallel] -> do 
                putStrLn "running agents in parallel..."
                start_p missions
            [NoHtml] -> do
                putStrLn "skipping html generation..."
                start missions
            [_]      -> putStr ""




----------------------------------------------------------------------
-- main entrypoint
--
-- note:
----------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        ([],    [],      [])   -> start missions 
        (flags, [],      [])   -> processArgs flags
        (_,     nonOpts, [])   -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,     _,       msgs) -> error $ concat msgs ++ usageInfo header options


