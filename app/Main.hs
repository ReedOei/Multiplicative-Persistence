module Main where

import System.Environment

import Persistence

search base = do
    let candidates = genCandidates base
    mapM_ print =<< persistence base candidates

main = do
    args <- getArgs
    case args of
        [baseStr] -> search $ read baseStr

