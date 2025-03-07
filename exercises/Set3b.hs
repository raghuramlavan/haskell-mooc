-- Exercise set 3b
--
-- This is a special exercise set. The exercises are about
-- implementing list functions using recursion and pattern matching,
-- without using any standard library functions. For this reason,
-- you'll be working in a limited environment where almost none of the
-- standard library is available.
--
-- At least the following standard library functions are missing:
--  * (++)
--  * head
--  * tail
--  * map
--  * filter
--  * concat
--  * (!!)
--
-- The (:) operator is available, as is list literal syntax [a,b,c].
--
-- Feel free to use if-then-else, guards, and ordering functions (< and > etc.).
--
-- The tests will check that you haven't added imports :)

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
--
-- Use recursion and the : operator to build the list.
--
-- Examples:
--   buildList 1 5 2 ==> [1,1,1,1,1,2]
--   buildList 7 0 3 ==> [3]

buildList :: Int -> Int -> Int -> [Int]
buildList start count end = buildList' start count [end]
    where
        buildList' s c  xs
         | c == 0 = xs
         | c > 0  = buildList' s (c-1) (start : xs )


------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Use recursion and the : operator to build the list.
--
-- Ps. you'll probably need a recursive helper function

sums :: Int -> [Int]
sums i = [(k *(k+1)) `div`2 | k <-[1.. i]]


------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   mylast 0 [] ==> 0:q
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def [] = def
mylast def [a] = a
mylast def (x:xs) = mylast def xs


------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   indexDefault [True] 1 False         ==>  False
--   indexDefault [10,20,30] 0 7         ==>  10
--   indexDefault [10,20,30] 2 7         ==>  30
--   indexDefault [10,20,30] 3 7         ==>  7
--   indexDefault ["a","b","c"] (-1) "d" ==> "d"

indexDefault :: [a] -> Int -> a -> a
indexDefault [] i def = def
indexDefault [x] i def = if i == 0 then x else def
indexDefault (x:xs) 0 def = x
indexDefault(x:xs) i def = if i<0 then def else indexDefault xs (i-1) def

------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
--
-- Use pattern matching and recursion to iterate through the list.

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:xs) = sorted' x xs
    where
        sorted' y1 (y2:ys) = (y1 <= y2) && sorted' y2 ys
        sorted' y1 [y2]    =  y1 <= y2
        sorted' y1 []      = True


------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
--
-- Use pattern matching and recursion (and the list constructors : and [])
sums2 :: [Int] -> Int
sums2 [y] = y
sums2 [] = 0
sums2 (y:ys) = y+ sums2 ys


sumsOf :: [Int] -> [Int]
sumsOf [] = []
sumsOf [a] =[a]
sumsOf x= sumOf' (rev [] x) []
    where
        sumOf' [] r = r
        sumOf' [a] r = a : r
        sumOf' (z:zs) r = sumOf' zs (sumAll (z : zs) : r)
        sumAll :: [Int]-> Int
        sumAll [] = 0
        sumAll [y] = y
        sumAll (y:ys) = y + sumAll ys
        rev acc [] = acc
        rev acc (x:xs) = rev (x:acc) xs




------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]

merge :: [Int] -> [Int] -> [Int]
merge xs ys = rev [] (merge' [] xs ys)
    where
        merge' acc (i:is) (j:js)
            | i <= j = merge' (i : acc) is (j:js)
            | i > j = merge' (j : acc) (i:is) js
        merge' acc [i] [j]
            | i <= j = merge' (i : acc) [] [j]
            | i> j   = merge' (j:acc) [i] []
        merge' acc [] (j:js) = merge' (j:acc) [] js
        merge' acc [] [j]    = j:acc
        merge' acc (i:is) []  = merge' (i:acc) is []
        merge' acc [i] []     = merge' (i:acc) [] []
        merge' acc [] []      = acc
        rev acc [] = acc
        rev acc (x:xs) = rev (x:acc) xs

------------------------------------------------------------------------------
-- Ex 8: define the function mymaximum that takes a list and a
-- function bigger :: a -> a -> Bool and returns the
-- biggest of the list, according to the comparing function.
--
-- An initial biggest value is provided to give you something to
-- return for empty lists.
--
-- Examples:
--   mymaximum (>) 3 [] ==> 3
--   mymaximum (>) 0 [1,3,2] ==> 3
--   mymaximum (>) 4 [1,3,2] ==> 4    -- initial value was biggest
--   mymaximum (<) 4 [1,3,2] ==> 1    -- note changed biggerThan
--   mymaximum (\xs ys -> length xs > length ys) [] [[1,2],[3]]
--     ==> [1,2]

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum bigger initial (x:xs)=  if bigger  x initial   then mymaximum  bigger x xs else mymaximum  bigger initial xs
mymaximum bigger initial [x]=  if bigger x initial   then x else initial
mymaximum bigger initial [] = initial
------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching. Do not use any library functions.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f as bs = rev' [] (map2' f as bs [])

map2' f (a:as) (b:bs) acc = map2' f as bs (f a b:acc)
map2' f (a:as) [b] acc =  f a b:acc
map2' f [a] (b:bs) acc =  f a b:acc
map2' f [a] [b] acc =  f a b:acc
map2' f [] _ acc = acc
map2' f _ [] acc = acc

rev' :: [a] -> [a] -> [a]
rev' acc [] = acc
rev' acc (x:xs) = rev' (x:acc) xs

------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap f xs = rev' [] (maybeMap' f xs [])
    where
        maybeMap' f (x:xs) r = if check (f x) then maybeMap' f xs  (get (f x):r) else  maybeMap' f xs r
        maybeMap' f [x] r    = if check (f x) then get (f x):r else  r
        maybeMap' f [] r =  r
        check (Just v) = True
        check Nothing  = False
        get (Just t) = t