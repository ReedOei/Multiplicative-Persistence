module Main where

import Persistence

main = mapM_ print =<< persistence genCandidates

