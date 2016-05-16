module Init where

import System.Random
import Lib
import Cities
import City
import Individual
import Population

max_ = 100.0

newMap :: Int -> IO Cities -- number of cities
newMap 0 = return []
newMap i = do
  gen <- newStdGen
  let [ x, y ] = take 2 $ randoms gen :: [ Float ] 
  let (_, gen') = next gen
  let city = newCity i (x*max_, y*max_)
  rest <- newMap (i-1) 

  return (city : rest)

createPopulation :: Int -> Int -> IO Population
createPopulation 0 _ = return []
createPopulation size nbrCities = do -- population size, number of cities
  individual <- createIndividual nbrCities
  rest <- createPopulation (size-1) nbrCities
  return (individual : rest)
