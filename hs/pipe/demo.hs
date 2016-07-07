module Main where

import Pipes ((>->), Pipe, Producer, Effect, runEffect, for, each, yield, await, discard)
import Pipes.Prelude (drain)
import Control.Monad ((>=>))
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.Trans.Class (lift)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Maybe (isJust)

chunkLength :: Int
chunkLength = 10

newtype BasePattern = BasePattern [Int] deriving (Show)
newtype Transformation = Transformation [Int] deriving (Show)

instance Monoid Transformation where
    mempty = Transformation []
    mappend (Transformation a) (Transformation b) = Transformation $ a ++ b

patternA = BasePattern [0,1,2,3,4]
patternB = BasePattern [0,2,4,6]
patternC = BasePattern [0,3,6]
patternD = BasePattern [0,4]

toTransformation :: BasePattern -> Transformation
toTransformation (BasePattern p) = Transformation $ do
    n <- [0..(chunkLength - 1)]
    x <- take (length p * chunkLength) p
    return $ (x + n) `mod` chunkLength

offsetTransformation :: Transformation -> Int -> Transformation
offsetTransformation (Transformation t) n = Transformation $ fmap (+ offset) t
    where offset = n * chunkLength

pipeWith :: (Int -> Transformation) -> Maybe Int -> Pipe (Maybe Int) (Maybe Int) (Writer [Transformation]) (Maybe Int)
pipeWith _ Nothing = yield Nothing >> return Nothing >> await
pipeWith f (Just a) = do
    mb <- await
    yield (Just a)
    if isJust mb then
        (lift . tell . return $ f a) >> return mb
    else
        return Nothing

toPipeSequence :: Transformation -> Pipe (Maybe Int) (Maybe Int) (Writer [Transformation]) (Maybe Int)
toPipeSequence transformation = do
    let applyOffset  = offsetTransformation transformation
        pipeSequence = foldr1 (>=>) (repeat $ pipeWith applyOffset)

    -- get the first number, and then hand it off to the pipe sequence
    await >>= pipeSequence

transformPipeline :: Pipe (Maybe Int) (Maybe Int) (Writer [Transformation]) ()
transformPipeline = foldr1 (>->) (fmap (toPipeSequence.toTransformation) [patternA, patternB, patternC, patternD]) >>= discard

pipeline :: Effect (Writer [Transformation]) ()
pipeline = spout >-> transformPipeline >-> drain

spout :: Monad m => Producer (Maybe Int) m ()
spout = for (each ns) yield
    where ns = [(Just x) | x <- [0..10]] ++ [Nothing]

main = let transformations = execWriter . runEffect $ pipeline in
       mapM_ (putStrLn . show) transformations
