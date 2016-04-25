module Lib where

import System.Random
import Data.List

rand :: (RandomGen g) => g -> [ a ] -> (a, g)
rand g xs =
  let (i, g') = randomR (0, length xs - 1) g in
  (xs !! i, g')

randList :: (RandomGen g) => g -> Int -> [ a ] -> ([ a ], g)
randList g n xs =
  let is = take n . nub $ randomRs (0, length xs - 1) g in
  let (_, g') = next g in
  (map (\ i -> xs !! i) is, g')
