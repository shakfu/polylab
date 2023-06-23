module Model

where

import Text.Feed.Types (Item, Feed)


data Entry = Entry
    { entryItem       :: Item
    , entryWords      :: [String]
    , entryRelevance  :: Float
    } deriving Show


data Target = Target
    { targetName      :: String
    , targetURL       :: String
    , targetKeywords  :: [String]
    , targetRelevance :: Float
    , targetData      :: Maybe String
    , targetFeed      :: Maybe Feed
    , targetEntries   :: Maybe [Entry]
    } deriving Show


data Mission = Mission
    { missionName     :: String
    , missionCodeName :: String
    , missionTargets  :: [Target]
    , nAgents         :: Integer
    } deriving Show

