{-# LANGUAGE OverloadedStrings #-}

module Views.Html

where

import Data.Maybe

import Text.Feed.Query

import Model
import Config
import Analysis (prioritize)
import Utils (info, showdec)


import Control.Monad (forM_)
--import Data.Monoid (mempty)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

mkLink :: (ToValue a, ToMarkup a1) => a -> a1 -> Html
mkLink url txt = a ! href (toValue url) $ toHtml txt

-- | Link to a CSS stylesheet.
css :: H.AttributeValue -> H.Html
css link = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href link

-- | Link to a javscript file.
js :: H.AttributeValue -> H.Html
js link = H.script ! A.type_ "text/javascript" ! A.src link $ ""

-- | Create a link.
linkTo :: H.AttributeValue -> H.Html -> H.Html
linkTo url = H.a ! A.href url

renderMissionInHtml m = do
    let filename = missionCodeName m
    let path = "./public/mission_" ++ filename ++ ".html"
    let html = showMission m
    writeFile path $ renderHtml html

showMission :: Mission -> Html
showMission m = docTypeHtml $ do
    -- let title = toHtml ("Mission: " ++ missionName m)
    let title = toHtml $ missionName m
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title title
        css "css/bootstrap.css"
        css "css/docs.css"
        js "js/jquery.js"
        js "js/bootstrap.min.js"
        js "js/application.js"

    H.body ! dataAttribute "spy" "scroll" ! dataAttribute "target" ".bs-docs-sidebar" $ do
        
        -- NAVBAR
        H.div ! class_ "navbar navbar-inverse navbar-fixed-top" $ do
            H.div ! class_ "navbar-inner" $ do
                H.div ! class_ "container" $ do
                    H.button ! A.type_ "button" ! class_ "btn btn-navbar" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".nav-collapse" $ do
                        H.span ! class_ "icon-bar" $ ""
                        H.span ! class_ "icon-bar" $ ""
                        H.span ! class_ "icon-bar" $ ""
                    H.a ! class_ "brand" ! A.href "index.html" $ "newsagent"
                    H.div ! class_ "nav-collapse collapse" $ do
                        H.ul ! class_ "nav" $ do
                            H.li $ linkTo "mission_tech.html" "tech"
                            H.li $ linkTo "mission_mideast.html" "mideast"
                    
        -- HEADER
        H.header ! class_ "jumbotron subhead" ! A.id "overview" $ do
        -- H.header $ do
            H.div ! class_ "container" $ do
                h1 title
                H.p ! class_ "lead" $ "introduction text / date"
        -- BODY
        H.div ! class_ "container" $ do
            H.div ! class_ "row" $ do
                H.div ! class_ "span3 bs-docs-sidebar" $ do
                    H.ul ! class_ "nav nav-list bs-docs-sidenav" $ do
                        forM_ (missionTargets m) showNavEntry
                H.div ! class_ "span9" $ do
                    forM_ (missionTargets m) showTarget

showNavEntry :: Target -> Html
showNavEntry t = do
    H.li $ do
        a ! href (toValue ("#" ++ targetName t)) $ do
            H.i ! class_ "icon-chevron-right" $ ""
            toHtml (targetName t)

showTarget :: Target -> Html
showTarget t = do
    H.section ! A.id anchor $ do
        H.div ! class_ "page-header" $ do
            h1 $ toHtml (targetName t)
        showTargetFeed t
    where 
        anchor = toValue (targetName t)


showTargetFeed :: Target -> Html
showTargetFeed t = do
    -- let feed = fromJust $ targetFeed t
    -- let title = getFeedTitle feed
    let feedRelevance = showdec $ targetRelevance t
    p $ toHtml ("relevance: " ++ feedRelevance)
    table ! class_ "table" $ do
        thead $ tr $ do
            th "REL"
            th "LINK"
        tbody $ mapM_ showEntry (prioritize $ fromJust $ targetEntries t)

showEntry :: Entry -> Html
showEntry e = do
    let item = entryItem e
    let itemTitle = fromJust $ getItemTitle item
    let itemLink = fromJust $ getItemLink item
    let rel = showdec $ entryRelevance e
    let nWords = length $ entryWords e
    tr $ do
        td $ toHtml rel
        td $ mkLink itemLink itemTitle 
