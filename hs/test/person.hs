{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-} 

import Data.String

-- translate the functor argument signature
class (Num (XDouble a), IsString (XString a)) => Ops a where
    type XString a
    type XDouble a
    opConcat :: a -> XString a -> XString a -> XString a
    opShow   :: a -> XDouble a -> XString a

-- translate the functor result signature
class Ops a => MkPersonSIG b a where
    data Person b a
    display :: b -> a -> Person b a -> XString a

-- translate the functor
data MkPerson = MkPerson
instance Ops a => MkPersonSIG MkPerson a where
    data Person MkPerson a = 
         Person { firstName :: XString a,
                  lastName  :: XString a,
                  height    :: XDouble a }
    display _ a p = 
        let (+++) = opConcat a
            xshow = opShow a
        in firstName p +++ lastName p +++ xshow (height p + 1)

-- create a concrete functor argument
data Basic = Basic
instance Ops Basic where
    type XString Basic = String
    type XDouble Basic = Double
    opConcat _ = (++)
    opShow _ = show

-- use the functor
main = let p = Person "John" "Smith" 150 
       in putStrLn $ display MkPerson Basic p