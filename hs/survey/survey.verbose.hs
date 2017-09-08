module Main where

import Text.Show.Functions
import Data.Maybe

type Name            = String
type QuestionText    = String
type Answer          = String
type Score           = Double
type CorrectAnswer a = a
type Option a        = (String, a)

-- type converters
str      = id
int s    = read s :: Int
double s = read s :: Double

data QuestionType   = Open
                    | Test 
                    | Choice 
                      deriving (Show, Eq)

data Question a = Question
    { questionName    :: Name
    , questionText    :: QuestionText
    , questionType    :: QuestionType
    , answerFunc      :: String -> a
    , correctAnswer   :: Maybe a
    , options         :: Maybe [Option a]
    } deriving (Show)

data Question' = QuestionS (Question String) 
               | QuestionI (Question Int) 
               | QuestionD (Question Double)  
                 deriving (Show)

data QuestionSet = QuestionSet
    { qsetTitle     :: String
    , qsetQuestions :: [Question']
    , qsetPoints    :: Double
    } deriving (Show)

data Survey = Survey
    { surveyTitle        :: String
    , surveyQuestionSets :: [QuestionSet]
    } deriving (Show)



parse :: Question a -> Answer -> a
parse = answerFunc

view  :: Question a -> String
view = questionName

askQ   :: Question a -> IO Answer
askQ q = do
    putStrLn $ questionText q
    getLine

askQ' :: Question' -> IO Answer
askQ' q = case q of
    QuestionS x -> askQS q
    QuestionI x -> askQI q
    QuestionD x -> askQD q
    where
        askQS q = askQ (extractQString q)
        askQI q = askQ (extractQInt    q)
        askQD q = askQ (extractQDouble q)

    

askQset :: QuestionSet -> IO [Answer]
askQset qs = mapM askQ' (qsetQuestions qs)
    
    
store :: Question a -> Answer -> IO ()
store q ans = putStrLn $ questionName q ++ ": " ++ show ans

{-
extract :: Question' -> Question a
extract q = case q of
    QuestionS x -> extractQString q
    QuestionI x -> extractQInt q
    QuestionD x -> extractQDouble q
-}

extractQString :: Question' -> Question String
extractQString (QuestionS q) = q

extractQInt :: Question' -> Question Int
extractQInt (QuestionI q) = q

extractQDouble :: Question' -> Question Double
extractQDouble (QuestionD q) = q

testQ :: (Eq a) => Question a -> Answer -> Bool
testQ q ans = case correctAnswer q of
    Nothing -> False
    Just x  -> x == answerFunc q ans

testQ' :: Question' -> Answer -> Bool
testQ' q a = case q of
    QuestionS x -> testQS q a
    QuestionI x -> testQI q a
    QuestionD x -> testQD q a
    where
        testQS q = testQ (extractQString q)
        testQI q = testQ (extractQInt    q)
        testQD q = testQ (extractQDouble q)


testQset :: QuestionSet -> [Answer] -> [Bool]
testQset qs = zipWith testQ' (qsetQuestions qs)


evalQset :: QuestionSet -> [Answer] -> Score
evalQset qs as = (total_correct / total_questions) * score
    where
        total_questions = fromIntegral (length $ qsetQuestions qset)
        total_correct = fromIntegral (length $ filter (== True) (testQset qset as))
        score = qsetPoints qset


q1 = Question
    { questionName  = "q1"
    , questionText  = "What is our name?"
    , questionType  = Open
    , answerFunc    = id
    , correctAnswer = Nothing
    , options       = Nothing
    }

q2 = Question
    { questionName  = "q2"
    , questionText  = "What is 1+1?"
    , questionType  = Test
    , answerFunc    = int
    , correctAnswer = Just 2
    , options       = Nothing
    }

q3 = Question
    { questionName  = "q3"
    , questionText  = "What is 2+1?"
    , questionType  = Choice
    , answerFunc    = int
    , correctAnswer = Just 3
    , options       = Just [("a", 2), ("b", 3), ("c", 4)]
    }

q4 = Question
    { questionName  = "q4"
    , questionText  = "What is 2.0 + 1.5 ?"
    , questionType  = Choice
    , answerFunc    = double
    , correctAnswer = Just 3.5
    , options       = Just [("a", 2.1), ("b", 3.5), ("c", 4.4)]
    }


qset = QuestionSet
    { qsetTitle     = "simple questions"
    , qsetQuestions = [ QuestionS q1 
                      , QuestionI q2 
                      , QuestionI q3
                      , QuestionD q4
                      ]
    , qsetPoints    = 100.0
    }

survey = Survey
    { surveyTitle        = "a survey"
    , surveyQuestionSets = [qset]
    }
    
t1 = evalQset qset ["1", "2", "3", "4"]

