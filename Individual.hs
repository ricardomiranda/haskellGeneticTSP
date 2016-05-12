module Individual where

import Data.Sequence
import Data.Foldable
import System.Random
import System.Random.Shuffle

-- A chromosome is a sequense cities to visit (order, name of the city)
data Individual = Individual { chromosome :: [ (Int, Int) ] -- [ (pos, gene) ]
                             , fitness :: Maybe Float }     -- total distance 
	            deriving Show

instance Eq Individual where
  Individual { fitness = a } == Individual { fitness = b } = Just a == Just b

instance Ord Individual where
  compare Individual { fitness = a } Individual { fitness = b } = compare (Just a) (Just b)

newIndividual :: [ (Int, Int) ] -> Individual
newIndividual gs = Individual { chromosome = gs
                              , fitness = Nothing }

createIndividual :: Int -> IO Individual
createIndividual n = do -- number of cities
  gen <- newStdGen
  let xs' = [ 1..n ]
  let xs = Prelude.zip [ 0.. ] (shuffle' xs' (Prelude.length xs') gen)
  return (newIndividual xs)

createIndividualConst :: Int -> Int -> Individual
createIndividualConst n const = -- number of cities
  let xs' = Prelude.replicate n const in
  let xs = Prelude.zip [ 0.. ] xs' in
  newIndividual xs

modifyChromosome :: Individual -> (Int, Int) -> Individual
modifyChromosome i g =
  i { chromosome = toList $ update (fst g) g (fromList $ chromosome i) }
