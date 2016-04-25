module Individual where

import System.Random
import System.Random.Shuffle

-- A chromosome is a sequense cities to visit (order, name of the city)
data Chromosome = Chromosome { genes :: [ (Int, Int) ] -- [ (pos, chromosome) ]
                             , fitness :: Maybe Int } -- total distance 
	            deriving Show

instance Eq Chromosome where
  Chromosome { fitness = a } == Chromosome { fitness = b } = Just a == Just b

instance Ord Chromosome where
  compare Chromosome { fitness = a } Chromosome { fitness = b } = compare (Just a) (Just b)

newChromosome :: [ (Int, Int) ] -> Maybe Int -> Chromosome
newChromosome gs f = Chromosome { genes = gs
                                , fitness = f }

createChromosome :: (RandomGen g) => g -> Int -> (Chromosome, g)
createChromosome g n =
  let xs' = [ 1..n ] in
  let xs = zip [ 1.. ] (shuffle' xs' (length xs) g) in
  let (_, g') = next g in
  (newChromosome xs Nothing, g')
