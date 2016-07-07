{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Notes where

import Prelude hiding (Functor(..))

{- 
Functor: A simple intuition is that a Functor represents a “container” of some sort, along with the ability to apply a function uniformly to every element in the container.
-}

class Functor f where
    fmap :: (a -> b) -> f a -> f b

{-

f is a type constructor (like Maybe)

From the container point of view, the intention is that fmap applies a function to each element of a container, without altering the structure of the container

From the context point of view, the intention is that fmap applies a function to a value without altering its context.

There are two fundamental ways to think about fmap. The first has already been mentioned: it takes two parameters, a function and a container, and applies the function “inside” the container, producing a new container. Alternately, we can think of fmap as applying a function to a value in a context (without altering the context).

-}

instance Functor [] where
    fmap _ [] = []
    fmap g (x:xs) = g x : fmap g xs

 --or we could just say fmap = map

instance Functor Maybe where 
    fmap _ Nothing = Nothing 
    fmap g (Just a) = Just (g a)

-- ------------------------------------------------------------------

class Functor f => Applicative f where 
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

