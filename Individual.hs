module Individual where

import System.Random
import System.Random.Shuffle

-- A chromosome is a sequense cities to visit (order, name of the city)
data Individual = Individual { chromosome :: [ (Int, Int) ] -- [ (pos, gene) ]
                             , fitness :: Maybe Float } -- total distance 
	            deriving Show

instance Eq Individual where
  Individual { fitness = a } == Individual { fitness = b } = Just a == Just b

instance Ord Individual where
  compare Individual { fitness = a } Individual { fitness = b } = compare (Just a) (Just b)

newIndividual :: [ (Int, Int) ] -> Maybe Float -> Individual
newIndividual gs f = Individual { chromosome = gs
                                , fitness = f }

createIndividual :: (RandomGen g) => g -> Int -> (Individual, g)
createIndividual g n =
  let xs' = [ 1..n ] in
  let xs = zip [ 1.. ] (shuffle' xs' (length xs) g) in
  let (_, g') = next g in
  (newIndividual xs Nothing, g')
