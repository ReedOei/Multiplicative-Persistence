module Persistence where

import Data.List

import Data.String.Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Math.NumberTheory.Primes

pair [] = []
pair [x] = []
pair (a:b:xs) = (a,b) : pair (b : xs)

digits = toBase 10

toBase b n
    | n < b = [n]
    | otherwise = m : toBase b d
    where (d, m) = n `divMod` b

base3 = map (head . toBase 3) primes

frequency :: (Ord a, Foldable t, Num b) => t a -> Map a b
frequency = foldl' (\m x -> Map.insertWith (+) x 1 m) Map.empty

sequences :: Read a => IO [[a]]
sequences = map (map read . filter (not . null) . tail . split ",") . drop 4 . lines <$> readFile "stripped"

numberFrequencies :: IO [[(Integer, Integer)]]
numberFrequencies = map (Map.toList . frequency) . concat . inits <$> sequences

-- persistenceStep = product . digits

-- persistence x
--     | x < 10 = [x]
--     | otherwise = x : persistence (product (digits x))

-- persistence x
--     | x < 10 = 1
--     | otherwise = 1 + persistence (product (digits x))

fromDigits = foldl (\a b -> 10*a + b) 0

concats _    0 = [[]]
concats prev n = [ x:rest | x <- [prev..3] ++ if prev >= 7 then [prev..9] else [], rest <- concats (if x <= 5 then 7 else x) (n - 1) ]

genCandidates = genCandidates' 233
    where
        genCandidates' n = map fromDigits (concats 1 n) ++ genCandidates' (n + 1)

persistence candidates = persistence' Map.empty candidates 0
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
                    | x < 10 = (vs, 1)
                    | otherwise =
                        case Map.lookup x vs of
                            Just per -> (vs, per)
                            Nothing -> let (final, per) = go vs $ product $ digits x
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

