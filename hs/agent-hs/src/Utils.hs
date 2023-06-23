module Utils

where

import Network.HTTP (getResponseBody, simpleHTTP, getRequest)
import Data.Time (getCurrentTime, defaultTimeLocale)
import Data.Time.Format (formatTime)
-- import System.Locale (defaultTimeLocale)
import Text.Printf

showdec :: Float -> String
showdec value = printf "%.2f" value

downloadURL :: String -> IO String
downloadURL url = getResponseBody =<< simpleHTTP (getRequest url)

anyMember :: (Eq t) => [t] -> [t] -> Bool
anyMember xs ys = any (== True) [x `elem` ys | x <- xs]

percentTrue :: (Fractional b) => [Bool] -> b
percentTrue xs  = trues / total
    where
        trues  = fromIntegral (length $ filter (== True) xs)
        total  = fromIntegral $ length xs

datetime :: IO String
datetime = do
    now <- getCurrentTime
    return $ formatTime defaultTimeLocale "%F %X" now

info' :: String -> String -> IO ()
info' action txt = do
    now <- datetime
    putStrLn $ now ++ "   " ++ action ++ "    " ++ txt

info :: String -> String -> IO ()
info action txt = do
    let p = pad 10
    putStrLn $ "    " ++ p action ++ txt
    where
        pad :: Int -> String -> String
        pad i s = if length s > i then s
            else s ++ concat (replicate (i - length s) " ")

