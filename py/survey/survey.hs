module Main where

import Text.Show.Functions

type Name            = String
type QuestionText    = String
type Score           = Double
type Option a        = (String, a)

data AnswerType = AnsD Double
                | AnsS String
                | AnsI Integer
                | AnsB Bool
                  deriving (Show, Read, Eq)

data Answer = AnyAnswer
            | TestAnswer AnswerType
            | MultipleChoice AnswerType [Option AnswerType]
              deriving (Show)

data Question = Question
    { questionName    :: Name
    , questionText    :: QuestionText
    , answerFunc      :: String -> AnswerType
    , answer          :: Answer
    } deriving (Show)


data QuestionSet = QuestionSet
    { qsetTitle     :: Name
    , qsetQuestions :: [Question]
    , qsetPoints    :: Score
    } deriving (Show)

data Survey = Survey
    { surveyTitle        :: Name
    , surveyQuestionSets :: [QuestionSet]
    } deriving (Show)


-- type converters
convert x = read x :: AnswerType
str s = convert ("AnsS \""++ s ++"\"")
int s = convert ("AnsI "++s)
double s = convert ("AnsD "++s)
bool s = convert ("AnsB "++s)

askQuestion   :: Question -> IO String
askQuestion q = do
    putStrLn $ questionText q
    getLine

askQuestionSet :: QuestionSet -> IO [String]
askQuestionSet qs = mapM askQuestion (qsetQuestions qs)

testQuestion :: Question -> String -> Bool
testQuestion q ans = case answer q of
    AnyAnswer           -> not (null ans)
    TestAnswer c        -> c == answerFunc q ans
    MultipleChoice c os -> c == answerFunc q ans

testQuestionSet :: QuestionSet -> [String] -> [Bool]
testQuestionSet qs = zipWith testQuestion (qsetQuestions qs)

takeQuestionSet :: QuestionSet -> IO [Bool]
takeQuestionSet qs = do
    answers <- askQuestionSet qs
    return (testQuestionSet qs answers)

scoreQuestionSet :: QuestionSet -> [String] -> Score
scoreQuestionSet qs as = (total_correct / total_questions) * score
    where
        total_questions = fromIntegral (length $ qsetQuestions qset)
        total_correct = fromIntegral (length $ filter (== True) (testQuestionSet qset as))
        score = qsetPoints qset


q1 = Question
    { questionName  = "q1"
    , questionText  = "What is our name?"
    , answerFunc    = str
    , answer        = AnyAnswer
    }

q2 = Question
    { questionName  = "q2"
    , questionText  = "What is 1+1?"
    , answerFunc    = int
    , answer        = TestAnswer (AnsI 2)
    }

q3 = Question
    { questionName  = "q3"
    , questionText  = "True or False: 10 * 2 = 21"
    , answerFunc    = bool
    , answer        = TestAnswer (AnsB False)
    }
    
q4 = Question
    { questionName  = "q4"
    , questionText  = "What is 2+1?"
    , answerFunc    = int
    , answer        = MultipleChoice (AnsI 3) [ ("a", AnsI 2)
                                              , ("b", AnsI 3)
                                              , ("c", AnsI 4)
                                              ]
    }

q5 = Question
    { questionName  = "q5"
    , questionText  = "What is 2.0 + 1.5 ?"
    , answerFunc    = double
    , answer        = MultipleChoice (AnsD 3.5) [ ("a", AnsD 2.1)
                                                , ("b", AnsD 3.5)
                                                , ("c", AnsD 4.4)
                                                ]
    }



qset = QuestionSet
    { qsetTitle     = "simple questions"
    , qsetQuestions = [ q1, q2, q3, q4, q5 ]
    , qsetPoints    = 100.0
    }

survey = Survey
    { surveyTitle        = "a survey"
    , surveyQuestionSets = [qset]
    }

answers = ["jon", "2", "True", "3", "3.5"]
t1 = testQuestionSet qset answers
t2 = scoreQuestionSet qset answers

