module Analysis

( extractWords
, parseFeedString
, prioritize
, relevance
, analyzeFeed
, analyzeFeedItem
) where

import Model

import Data.Maybe
import Data.Text (unpack)
import Data.List (sortBy)

import Utils (info, downloadURL)
import Text (cleanWordList)

import Text.Feed.Types
import Text.Feed.Query
import Text.Feed.Import (parseFeedString)

import Stemming (stemWords)

import Text.HTML.TagSoup
import Text.StringLike


----------------------------------------------------------------------
-- Text Preparation and Processing
--
----------------------------------------------------------------------
extractWords :: String -> [String]
extractWords s = concatMap cleanWordList txts
    where
        tags = parseTags s
        txts = map fromTagText (filter isTagText tags)

----------------------------------------------------------------------
-- ANALYTICAL OPERATIONS: feed analysis and relevance assessment
--
-- note: an entry is the unit of analysis of a feed item
----------------------------------------------------------------------
relevance :: [String] -> [String] -> Float
relevance terms txt  = nhits / nterms
    where
        analysis = [i `elem` txt | i <- terms]
        nterms = fromIntegral (length terms)
        nhits = fromIntegral (length $ takeWhile (== True) analysis)


prioritize :: [Entry] -> [Entry]
prioritize entries = reverse $ sortBy compare entries
    where
        compare Entry {entryRelevance = r1}
                Entry {entryRelevance = r2}
            | r1 < r2 = LT
            | otherwise = GT


analyzeFeed :: Target -> IO [Entry]
analyzeFeed target = do
    let feed = fromJust $ targetFeed target
    let keywords = targetKeywords target
    let items = feedItems feed
    mapM (analyzeFeedItem keywords) items


analyzeFeedItem :: [String] -> Item -> IO Entry
analyzeFeedItem kwds item = do
--    let itemTitle   = fromJust $ getItemTitle item
--    let itemLink    = fromJust $ getItemLink item
    let itemSummary = unpack $ fromJust $ getItemSummary item
    let ws = extractWords itemSummary
    let r = relevance kwds ws
    let entry = Entry { entryItem=item
                      , entryWords=ws
                      , entryRelevance=r
                      }
    return entry


