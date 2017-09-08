{-# LANGUAGE ExistentialQuantification #-} 

module Main where

import Text.Show.Functions

type Name            = String
type QuestionText    = String
type AnswerFunc a    = (String -> a)
type CorrectAnswer a = a
type Option a        = (String, a)

-- type converters
str = id
int s = read s :: Int
float s = read s :: Double

data QuestionType   = Open
                    | Test 
                    | Choice 
                      deriving (Show, Eq)

class Questionable a where 
    ask :: a -> IO ()
    new :: a -> Question


instance Questionable Question where
    ask (Question typ name txt afunc canswer opts) = putStrLn "ask"
    new (Question typ name txt afunc canswer opts) = Question
        { questionType    = typ
        , questionName    = name
        , questionText    = txt
        , answerFunc      = afunc
        , correctAnswer   = canswer
        , options         = opts
        } --deriving (Show)

--data Question a = Question
data Question = forall a. Questionable a => Question
    { questionType    :: QuestionType
    , questionName    :: Name
    , questionText    :: QuestionText
    , answerFunc      :: AnswerFunc a
    , correctAnswer   :: Maybe (CorrectAnswer a)
    , options         :: Maybe [Option a]
    } --deriving (Show)

--q1 = new (Question Open "q1" "What is your name?" str Nothing Nothing)

