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
prod cmpSym l1 l2 = getfrontier initfrontier
  where initfrontier :: Frontier
        initfrontier = [(initval, 0, 0)]
        initval = (head l1) ++ (head l2)

getfrontier :: Frontier -> [Symbols]
getfrontier [] = []
getfrontier frontier = a: getfrontier frontier'''
  where ((a,r,c) : frontier') = frontier
        frontier''            = addrow frontier'  (r+1) c
        frontier'''           = addcol frontier'' r     (c+1)

addrow :: Frontier -> Int -> Int -> Frontier
addrow fs r c = if r `elem` rs then fs
                else insert r c fs
                where rs = map (\(_, r, _) -> r) fs

addcol :: Frontier -> Int -> Int -> Frontier
addcol fs r c = if c `elem` cs then fs
                else insert r c fs
                where cs = map (\(_, _, c) -> c) fs

insert :: Int -> Int -> Frontier -> Frontier
insert r c frontier =
  case nth r l1 of
    Nothing -> frontier
    (Just a) -> case nth c l2 of
      Nothing -> frontier
      (Just b) -> insertBy cmpVal (a ++ b, r, c) frontier

nth :: Int -> [a] -> Maybe a
nth 0 (x:_) = Just x
nth n (_:xs) | n > 0 = nth (n-1) xs
nth _ [] = Nothing

l1 = "a" : map (++"a") l1
l2 = "b" : map (++"b") l2
ntS = l1 +++ l2

main :: IO ()
main = mapM_ print (take 100 ntS)
