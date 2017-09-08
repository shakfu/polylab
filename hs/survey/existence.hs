{-# LANGUAGE ExistentialQuantification #-} 
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Text.Show.Functions

-- dummy funcs for now
class Question_ a where
    ask        :: a -> String
    view       :: a -> String
    parse      :: a -> String

data Question = forall a. Question_ a => Question a

type Name           = String
type QuestionString = String
type AnswerType     = String
type CorrectAnswer  = String
type Option         = (String, String)
type Options        = [Option]

-- question types
data Open      = Open   Name QuestionString AnswerType
data Test      = Test   Name QuestionString AnswerType CorrectAnswer
data Choice    = Choice Name QuestionString AnswerType CorrectAnswer Options

instance Question_ Open where
    ask   (Open n q a) = n
    view  (Open n q a) = q
    parse (Open n q a) = a

instance Question_ Test where
    ask   (Test n q a c) = n ++ c
    view  (Test n q a c) = q ++ c
    parse (Test n q a c) = a ++ c

instance Question_ Choice where
    ask   (Choice n q a c os) = n ++ c 
    view  (Choice n q a c os) = q ++ c
    parse (Choice n q a c os) = a ++ c

instance Question_ Question where
    ask   (Question q) = ask   q
    view  (Question q) = view  q
    parse (Question q) = parse q

--
-- Smart constructor
--

open :: Name -> QuestionString -> AnswerType -> Question
open n q a = Question (Open n q a)

test :: Name -> QuestionString -> AnswerType -> CorrectAnswer -> Question
test n q a c = Question (Test n q a c)

choice :: Name -> QuestionString -> AnswerType -> CorrectAnswer -> Options -> Question
choice n q a c os = Question (Choice n q a c os)

questions :: [Question]
questions = [ open   "q1" "what is your name" "str"
            , test   "q2" "what is 1+1?"      "int" "2"
            , choice "q3" "what is 2+2?"      "int" "4" [("a", "4")]
            ]
