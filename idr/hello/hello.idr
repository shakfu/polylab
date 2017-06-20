module Main
import Data.Vect

%flag C "-O3"

infixl 4 >->
(>->) : Bool -> Bool -> Bool
(>->) True b2 = b2
(>->) False b2 = False


record Person where
    constructor MkPerson
    firstName, middleName, lastName : String
    age : Int

fred : Person
fred = MkPerson "Fred" "Joe" "Bloggs" 30


record Class where
    constructor ClassInfo
    students : Vect n Person
    className : String

addStudent1 : Person -> Class -> Class
addStudent1 p c = record { students = p :: students c } c

addStudent2 : Person -> Class -> Class
addStudent2 p c = record { students $= (p ::) } c


class1 : Class
class1 = addStudent1 fred (ClassInfo [] "CS")

class2 : Class
class2 = addStudent2 fred (ClassInfo [] "CS")

record SizedClass (size : Nat) where
    constructor SizedClassInfo
    students : Vect size Person
    className : String

addStudent : Person -> SizedClass n -> SizedClass (S n)
addStudent p c =  SizedClassInfo (p :: students c) (className c)

parameters (x : Nat, y : Nat)
  add_all : Nat -> Nat
  add_all z = x + y + z


parameters (y : Nat, xs : Vect x a)
  data Vects : Type -> Type where
    MkVects : Vect y a -> Vects a

  append : Vects a -> Vect (x + y) a
  append (MkVects ys) = xs ++ ys


main : IO ()
main = putStrLn "Hello World"
