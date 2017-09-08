{-# LANGUAGE GADTs #-}

module Main where

type Name            = String
type QuestionText    = String
type Option a        = (String, a)
type Answer          = String
type Score           = Double

-- type converters
str      = id
int s    = read s :: Int
double s = read s :: Double


data Question a  where
	QuestionS :: Name 
			  -> QuestionText
			  -> (a -> String)
			  -> Maybe a
			  -> Maybe [Option a]
			  -> Question String
	QuestionI :: Name 
			  -> QuestionText
			  -> (a -> String)
			  -> Maybe a
			  -> Maybe [Option a]
			  -> Question Int


--extract :: Question a -> String
--extract (QuestionS i txt af ca opts) = i
--extract (QuestionI i txt af ca opts) = i


--data QuestionSet a = QuestionSet
--    { qsetTitle     :: String
--    , qsetQuestions :: [Question a]
--    , qsetPoints    :: Double
--    } deriving (Show)

--data Survey a = Survey
--    { surveyTitle        :: String
--    , surveyQuestionSets :: [QuestionSet a]
--    } deriving (Show)

--  