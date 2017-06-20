{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Demo where

import Control.Arrow

type Circuit = (->)


nandGate :: Circuit (Bool, Bool) Bool
nandGate = \(x, y) -> not (x && y)

wire :: Circuit Bool Bool
wire = id

splittedWire :: Circuit Bool (Bool, Bool)
splittedWire = (wire &&& wire)

inverter :: Circuit Bool Bool
inverter = splittedWire >>> nandGate

parallelInverters :: Circuit (Bool, Bool) (Bool, Bool)
parallelInverters = inverter *** inverter

orGate :: Circuit (Bool, Bool) Bool
orGate = parallelInverters >>> nandGate

orgGate1 = proc (a, b) -> do
    m1 <- nandGate -< (a, a)
    m2 <- nandGate -< (b, b)
    nandGate -< (m1, m2)

org :: Circuit (Bool, Bool) Bool
org = orgGate1

data Test a = Test Int a [a]
    deriving (Show, Functor, Foldable, Traversable)

data Survey a = Survey
    { title ::  String
    , questions :: [a]
    } deriving Show

instance Functor Survey where
    fmap f survey = survey { questions = fmap f (questions survey) }

--instance Applicative Survey where
--    -- pure x = Survey {title="", questions=[x]}
--    survey1 <*> survey2 = 

surveyS = Survey "str survey" ["a", "b", "c"]
surveyI = Survey "int survey" [1, 2, 3]

