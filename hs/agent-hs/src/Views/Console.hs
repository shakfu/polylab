module Views.Console

where

import Data.Maybe
import Data.Text (unpack)
import Text.Feed.Query

import Model
import Analysis (prioritize)
import Utils (info, showdec)


----------------------------------------------------------------------
-- Views are functions which display objects and return nothing
--
-- eg :: Target -> IO ()
----------------------------------------------------------------------

showMission :: Mission -> IO ()
showMission m =
    info "Mission:" (missionName m)

showTarget :: Target -> IO ()
showTarget t =
    if targetRelevance t > 0 then showTargetFeed t else
        putStrLn ""
        -- info (targetName t) "is irrelevant."

showTargetFeed :: Target -> IO ()
showTargetFeed t = do
    putStrLn ""
    let feed = fromJust $ targetFeed t
    let title = unpack $ getFeedTitle feed
    -- let items = feedItems feed
    info (showdec $ targetRelevance t) title
    putStrLn (concat $ replicate 100 "-")
    mapM_ showEntry (prioritize $ fromJust $ targetEntries t)

showEntry :: Entry -> IO ()
showEntry e = do
    let item = entryItem e
    let itemTitle = unpack $ fromJust $ getItemTitle item
    let rel = entryRelevance e
    let nWords = length $ entryWords e
    info (showdec rel) itemTitle
