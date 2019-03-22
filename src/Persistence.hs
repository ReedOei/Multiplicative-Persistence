module Persistence where

import Data.List

import Data.String.Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Math.NumberTheory.Primes

pair [] = []
pair [x] = []
pair (a:b:xs) = (a,b) : pair (b : xs)

toBase b n
    | n < b = [n]
    | otherwise = m : toBase b d
    where (d, m) = n `divMod` b

-- persistenceStep = product . digits

-- persistence x
--     | x < 10 = [x]
--     | otherwise = x : persistence (product (digits x))

-- persistence x
--     | x < 10 = 1
--     | otherwise = 1 + persistence (product (digits x))

fromDigits base = foldl (\a b -> base*a + b) 0

calcVals base = (highMin, max)
    where
        max = base - 1
        highMin = maximum $ filter (`notElem` products) [1..max]
        products = [ a * b | a <- [2..max], b <- [2..max] ]

candidates _      _       _      _    0 = [[]]
candidates curLow highMin maxDig prev n = do
    x <- [prev..maxDig]
    let newCurLow = if curLow <= maxDig then curLow * x else curLow
    let newPrev = if curLow > maxDig then max highMin x else x
    rest <- candidates newCurLow highMin maxDig newPrev (n - 1)
    pure $ x:rest

genCandidates base = genCandidates' 1
    where
        (highMin, maxDig) = calcVals base
        genCandidates' n = map (fromDigits base) (candidates 1 highMin maxDig 1 n) ++ genCandidates' (n + 1)

persistence base candidates = persistence' Map.empty candidates 0
    where
        persistence' known (x:xs) n
            | per > n = do
                putStrLn $ "\r" ++ show (x,per)
                (:) <$> pure (x,per) <*> persistence' newKnown xs (n + 1)
            | otherwise = do
                putStr $ "\r" ++ show (length (show x), per)
                persistence' newKnown xs n
            where
                (newKnown, per) = go known x
                go vs x
                    | x < 10 = (vs, 0)
                    | otherwise =
                        case Map.lookup x vs of
                            Just per -> (vs, per)
                            Nothing -> let (final, per) = go vs $ product $ toBase base x
                                       in (Map.insert x (per + 1) final, per + 1)

showMaxBy f (x:xs) = showMaxBy' (f x) xs
    where
        showMaxBy' cur (y:ys)
            | v > cur = do
                putStrLn $ "\r" ++ show y
                showMaxBy' v ys
            | otherwise = do
                putStr $ "\r" ++ show y
                showMaxBy' cur ys
            where v = f y

