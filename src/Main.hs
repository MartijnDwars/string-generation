{-# LANGUAGE PostfixOperators #-}

module Main where

import Data.List hiding (insert)

type Symbols = String
type Corner = (Symbols, Int, Int)
type Frontier = [Corner]

infixl 5 |||
infixl 6 +++

merge2 :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge2 cmp as [] = as
merge2 cmp [] bs = bs
merge2 cmp (a:as) (b:bs) = case cmp a b of
  GT -> b:(merge2 cmp (a:as) bs)
  _  -> a:(merge2 cmp as (b:bs))

(|||) :: [String] -> [String] -> [String]
a ||| b = merge2 cmpSymLen a b

cmpSymLen :: String -> String -> Ordering
cmpSymLen [] _ = LT
cmpSymLen _ [] = GT
cmpSymLen (_:as) (_:bs) = cmpSymLen as bs

cmpVal :: Corner -> Corner -> Ordering
cmpVal (v1, _, _) (v2, _, _) = cmpSymLen v1 v2

(+++) :: [Symbols] -> [Symbols] -> [Symbols]
a +++ b = prod cmpSymLen a b

prod :: (Symbols -> Symbols -> Ordering) -> [Symbols] -> [Symbols] -> [Symbols]
prod cmpSym l1 l2 = getfrontier initfrontier l1 l2
  where initfrontier :: Frontier
        initfrontier = [(initval, 0, 0)]
        initval = (head l1) ++ (head l2)

getfrontier :: Frontier -> [Symbols] -> [Symbols] -> [Symbols]
getfrontier []       l1 l2 = []
getfrontier frontier l1 l2 = a: getfrontier frontier''' l1 l2
  where ((a,r,c) : frontier') = frontier
        frontier''            = addrow frontier'  (r+1) c     l1 l2
        frontier'''           = addcol frontier'' r     (c+1) l1 l2

addrow :: Frontier -> Int -> Int -> [Symbols] -> [Symbols] -> Frontier
addrow fs r c l1 l2 = if r `elem` rs then fs
                else insert r c fs l1 l2
                where rs = map (\(_, r, _) -> r) fs

addcol :: Frontier -> Int -> Int -> [Symbols] -> [Symbols] -> Frontier
addcol fs r c l1 l2 = if c `elem` cs then fs
                else insert r c fs l1 l2
                where cs = map (\(_, _, c) -> c) fs

insert :: Int -> Int -> Frontier -> [Symbols] -> [Symbols] -> Frontier
insert r c frontier l1 l2 =
  case nth r l1 of
    Nothing -> frontier
    (Just a) -> case nth c l2 of
      Nothing -> frontier
      (Just b) -> insertBy cmpVal (a ++ b, r, c) frontier

nth :: Int -> [a] -> Maybe a
nth 0 (x:_) = Just x
nth n (_:xs) | n > 0 = nth (n-1) xs
nth _ [] = Nothing

flatMap :: (t -> [a]) -> [t] -> [a]
flatMap _ [] = []
flatMap f (x:xs) = f x ++ flatMap f xs

prependAll :: String -> [String] -> [String]
prependAll word = map (word ++)

(^*) :: [String] -> [String]
(^*) base = "" : (base^+)

(^+) :: [String] -> [String]
(^+) base = base ++ flatMap (flip prependAll base) (base^+)

ntA = "a" : map (++"a") ntA
ntB = "b" : map (++"bb") ntB

ntC :: [Symbols]
ntC = "c" : map (++"c") ntC

ntS = ((ntA +++ ntB)^*)

main :: IO ()
main = mapM_ print (take 100 ntS)
