module Lib where

import System.Random
import Data.List

randList :: Int -> [ a ] -> IO [ a ]
randList n xs = do
  gen <- newStdGen
  let is = take n . nub $ randomRs (0, length xs - 1) gen :: [ Int ]
  return (map (\ i -> xs !! i) is)
