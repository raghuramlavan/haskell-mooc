{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
-- Exercise set 6: defining classes and instances

module Set6 where

import Mooc.Todo
import Data.Char (toLower)
import Distribution.Simple.Utils (xargs)


------------------------------------------------------------------------------
-- Ex 1: define an Eq instance for the type Country below. You'll need
-- to use pattern matching.

data Country = Finland | Switzerland | Norway
  deriving Show

instance Eq Country where
  (==) Finland Finland = True
  (==) Switzerland Switzerland = True
  (==) Norway Norway = True
  (==) _  _ = False


------------------------------------------------------------------------------
-- Ex 2: implement an Ord instance for Country so that
--   Finland <= Norway <= Switzerland
--
-- Remember minimal complete definitions!

instance Ord Country where
  compare Finland Norway = LT
  compare Norway Switzerland = LT
  compare Finland Switzerland = LT
  compare Finland Finland = EQ
  compare Norway Norway = EQ
  compare Switzerland Switzerland = EQ
  compare Switzerland Norway = GT
  compare Switzerland Finland = GT
  compare Norway Finland = GT

------------------------------------------------------------------------------
-- Ex 3: Implement an Eq instance for the type Name which contains a String.
-- The Eq instance should ignore capitalization.
--
-- Hint: use the function Data.Char.toLower that has been imported for you.
--
-- Examples:
--   Name "Pekka" == Name "pekka"   ==> True
--   Name "Pekka!" == Name "pekka"  ==> False

data Name = Name String
  deriving Show

instance Eq Name where
  (==) (Name x) (Name y)
    | [toLower xx | xx <- x] == [toLower yy |yy <- y]= True
    | otherwise = False

------------------------------------------------------------------------------
-- Ex 4: here is a list type parameterized over the type it contains.
-- Implement an instance "Eq (List a)" that compares the lists element
-- by element.
--
-- Note how the instance needs an Eq a constraint. What happens if you
-- remove it?

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
  (==) Empty Empty = True
  (==) (LNode x y) (LNode x2 y2) = x==x2 && y ==y2
  (==) _ _ = False




------------------------------------------------------------------------------
-- Ex 5: below you'll find two datatypes, Egg and Milk. Implement a
-- type class Price, containing a function price. The price function
-- should return the price of an item.
--
-- The prices should be as follows:
-- * chicken eggs cost 20
-- * chocolate eggs cost 30
-- * milk costs 15 per liter
--
-- Example:
--   price ChickenEgg  ==>  20

data Egg = ChickenEgg | ChocolateEgg
  deriving Show
data Milk = Milk Int -- amount in litres
  deriving Show

class Price a where
  price :: a -> Int

instance Price Egg where
  price ChickenEgg = 20
  price ChocolateEgg = 30

instance Price Milk where
  price (Milk x) = x*15

instance Price (Maybe Milk) where
  price (Just x) = price x
  price Nothing  =0
------------------------------------------------------------------------------
-- Ex 6: define the necessary instances in order to be able to compute these:
--
-- price (Just ChickenEgg) ==> 20
-- price [Milk 1, Milk 2]  ==> 45
-- price [Just ChocolateEgg, Nothing, Just ChickenEgg]  ==> 50
-- price [Nothing, Nothing, Just (Milk 1), Just (Milk 2)]  ==> 45

instance Price (Maybe Egg) where
  price (Just ChickenEgg) = 20
  price (Just ChocolateEgg) = 30
  price Nothing = 0

instance Price [Egg] where
  price [] = 0
  price (x:xs) = price x  + price xs

instance Price [Maybe Egg] where
  price [] = 0
  price (x:xs) = price x  + price xs

instance Price [Maybe Milk] where
  price [] = 0
  price (x:xs) = price x  + price xs
------------------------------------------------------------------------------
-- Ex 7: below you'll find the datatype Number, which is either an
-- Integer, or a special value Infinite.
--
-- Implement an Ord instance so that finite Numbers compare normally,
-- and Infinite is greater than any other value.

data Number = Finite Integer | Infinite
  deriving (Show,Eq)

instance Ord Number where
  compare (Finite x) (Finite y) = compare x y
  compare (Finite x) Infinite   =  LT
  compare Infinite (Finite x)   =  GT
  compare _ _   = LT


------------------------------------------------------------------------------
-- Ex 8: rational numbers have a numerator and a denominator that are
-- integers, usually separated by a horizontal bar or a slash:
--
--      numerator
--    -------------  ==  numerator / denominator
--     denominator
--
-- You may remember from school that two rationals a/b and c/d are
-- equal when a*d == b*c. Implement the Eq instance for rationals
-- using this definition.
--
-- You may assume in all exercises that the denominator is always
-- positive and nonzero.
--
-- Examples:
--   RationalNumber 4 5 == RationalNumber 4 5    ==> True
--   RationalNumber 12 15 == RationalNumber 4 5  ==> True
--   RationalNumber 13 15 == RationalNumber 4 5  ==> False

data RationalNumber = RationalNumber Integer Integer
  deriving Show

instance Eq RationalNumber where
  (==) (RationalNumber x1 y1) (RationalNumber x2 y2) = x1*y2 == x2*y1

------------------------------------------------------------------------------
-- Ex 9: implement the function simplify, which simplifies rational a
-- number by removing common factors of the numerator and denominator.
-- In other words,
--
--     ca         a
--    ----  ==>  ---
--     cb         b
--
-- As a concrete example,
--
--     12        3 * 4         4
--    ----  ==  -------  ==>  ---.
--     15        3 * 5         5
--
-- Hint: Remember the function gcd?

simplify :: RationalNumber -> RationalNumber
simplify (RationalNumber x y) = RationalNumber (x `div` gcd (max (abs x) (abs y)) (min (abs x) (abs y))) (y `div` gcd (max  (abs x) (abs y)) (min  (abs x) (abs y)))
  where
    gcd x 0 = x
    gcd x y = gcd (max (x-y) y) (min (x-y) y)

------------------------------------------------------------------------------
-- Ex 10: implement the typeclass Num for RationalNumber. The results
-- of addition and multiplication must be simplified.
--
-- Reminders:
--   * negate x is 0-x
--   * abs is absolute value
--   * signum is -1, +1 or 0 depending on the sign of the input
--
-- Examples:
--   RationalNumber 1 3 + RationalNumber 1 6 ==> RationalNumber 1 2
--   RationalNumber 1 3 * RationalNumber 3 1 ==> RationalNumber 1 1
--   negate (RationalNumber 2 3)             ==> RationalNumber (-2) 3
--   fromInteger 17 :: RationalNumber        ==> RationalNumber 17 1
--   abs (RationalNumber (-3) 2)             ==> RationalNumber 3 2
--   signum (RationalNumber (-3) 2)          ==> RationalNumber (-1) 1
--   signum (RationalNumber 0 2)             ==> RationalNumber 0 1

instance Num RationalNumber where
  (+) (RationalNumber x1 y1) (RationalNumber x2 y2) = simplify (RationalNumber (x1*y2+x2*y1) (y1*y2))
  (*) (RationalNumber x1 y1) (RationalNumber x2 y2) = simplify (RationalNumber (x1*x2) (y1*y2))
  abs (RationalNumber x1 y1)= if x1 < 0 then RationalNumber (negate x1) y1 else RationalNumber x1 y1
  signum (RationalNumber x1 y1)= if x1 < 0 then RationalNumber (-1) 1 else RationalNumber 1 1
  fromInteger x = RationalNumber x 1
  negate (RationalNumber x1 y1) = RationalNumber (negate x1) y1

------------------------------------------------------------------------------
-- Ex 11: a class for adding things. Define a class Addable with a
-- constant `zero` and a function `add`. Define instances of Addable
-- for Integers and lists. Numbers are added with the usual addition,
-- while lists are added by catenating them. Pick a value for `zero`
-- such that: `add zero x == x`
--
-- Examples:
--   add 1 2                ==>  3
--   add 1 zero             ==>  1
--   add [1,2] [3,4]        ==>  [1,2,3,4]
--   add zero [True,False]  ==>  [True,False]

class Addable a where
  zero :: a
  add :: a->a->a

instance Addable [a] where
  zero = []
  add x y = x ++ y

instance  Addable Integer where
  zero = 0
  add x y = x + y
------------------------------------------------------------------------------
-- Ex 12: cycling. Implement a type class Cycle that contains a
-- function `step` that cycles through the values of the type.
-- Implement instances for Color and Suit that work like this:
--
--   step Red ==> Green
--   step Green ==> Blue
--   step Blue ==> Red
--
-- The suit instance should cycle suits in the order Club, Spade,
-- Diamond, Heart, Club.
--
-- Also add a function `stepMany` to the class and give it a default
-- implementation using `step`. The function `stepMany` should take
-- multiple (determined by an Int argument) steps like this:
--
--   stepMany 2 Club ==> Diamond
--   stepMany 3 Diamond ==> Spade
--
-- The tests will test the Cycle class and your default implementation
-- of stepMany by adding an instance like this:
--
--    instance Cycle Int where
--      step = succ

data Color = Red | Green | Blue
  deriving (Show, Eq)
data Suit = Club | Spade | Diamond | Heart
  deriving (Show, Eq)

class Cycle a where
  step :: a->a
  stepMany ::Int-> a-> a
  stepMany x a = if x == 0 then a else stepMany (x - 1) (step a) 

instance Cycle Color where
  step = succ2 where 
      succ2 Red = Green
      succ2 Green = Blue
      succ2 Blue = Red
  

instance Cycle Suit where
  step = succ2 where 
    succ2 Club= Spade
    succ2 Spade = Diamond
    succ2 Diamond = Heart
    succ2 Heart = Club
