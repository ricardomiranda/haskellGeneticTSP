module Population where


import Data.List
import Data.Maybe
import System.Random
import Cities
import City
import Individual
import Lib

-- A chromosome is a sequense of prof, time slot, room 
type Population = [ Individual ]

calcTotalDistance :: Cities -> Float
calcTotalDistance [] = error "Unsuficient number of cities, module Popoulation, function calcFitness"
calcTotalDistance [_] = 0
calcTotalDistance (c1:c2:cs) = squareDistance c1 c2
                             + calcTotalDistance (c2:cs)
  
calcFitness :: Cities -> Population -> Population
calcFitness _ [] = []
calcFitness cs (i:is) = 
  i { fitness = Just (calcTotalDistance (map (findCity cs . snd) (chromosome i))) } 
  : calcFitness cs is

calcFitnessPopulation :: Population -> Float
calcFitnessPopulation p = 
  sum $ map fit p
    where
      fit x = fromMaybe (error "Clashes unknown, module Population, function calcFitnessPopulation") 
                        (fitness x)

ordPopulation :: Population -> Population
ordPopulation = sort

tournamentSelection :: Int -> Population -> IO Individual
tournamentSelection n p = do -- tournament size, previous population
  -- Tournament selection selects its parents by running a series of "tournaments".
  -- First, individuals are randomly selected from the population and entered into a
  -- tournament. Next, these individuals can be thought to compete with each other
  -- by comparing their fitness values, then choosing the individual with the highest
  -- fitness for the parent.
  is <- randList n p
  return (head $ ordPopulation is)

selectParents :: Int -> Population -> IO (Individual, Individual)
selectParents n p = do -- tournament size, previous population
  parent1 <- tournamentSelection n p
  parent2 <- tournamentSelection n p
  return (parent1, parent2)

crossover :: Float -> (Individual, Individual) -> IO Individual
crossover c parents = do -- crossover rate, (first parent, second parent)
  gen <- newStdGen
  let r = head $ take 1 $ randoms gen :: Float
  if c < r 
    then do
        return (fst parents)
    else do
      let emptyGene = -1 
      gen' <- newStdGen
      let rs = take 2 $ randomRs (0, length (chromosome $ fst parents) - 1) gen' :: [ Int ]
      let pos1 = minimum rs
      let pos2 = maximum rs

      let ( _, chromosomeFstParent ) = unzip (chromosome $ fst parents) 
      let ( _, chromosomeSndParent ) = unzip (chromosome $ snd parents) 

      -- fst parent contribution
      let fstParentContrib = drop pos1 $ take pos2 $ chromosomeFstParent
      let child =  (take pos1 $ chromosomeSndParent)
                ++ (fstParentContrib)
		++ (drop pos2 $ chromosomeSndParent)

      return (fst parents)

{-
crossover :: Float -> (Individual, Individual) -> IO Individual
crossover c parents = do -- crossover rate, (first parent, second parent)
  -- With the traveling salesman problem, both the genes and the order of the genes
  -- in the chromosome are very important. In fact, for the traveling salesman 
  -- problem we shouldn't ever have more than one copy of a specific gene in our
  -- chromosome. This is because it would create an invalid solution because a city
  -- shouldn't be visited more than once on a given route. Consider a case where we
  -- ve three cities: City A, City B and City C. A route of A,B,C is valid; however,
  -- a route of C,B,C is not: this route visits City C twice, and also never visits
  -- City A. Because of this, it's essential that we find and apply a crossover 
  -- method that produces valid results for our problem.
  -- We also need to be respectful of the ordering of the parent's chromosomes
  -- during the crossover process. This is because the order of the chromosome
  -- the solution's fitness. In fact, it’s only the order that matters.
  -- Here we will aply ordered crossover.
  gen <- newStdGen
  let r = head $ take 1 $ randoms gen :: Float
  if c < r 
    then do
        return (snd parents)
    else do
      let emptyGene = -99
      gen' <- newStdGen
      let rs = take 2 . nub $ randomRs (0, length (chromosome $ fst parents) - 1) gen' :: [ Int ]
      let pos1 = minimum rs
      let pos2 = maximum rs
      let auxIndividual = createIndividualConst (length (chromosome $ fst parents)) emptyGene

      -- fst parent contribution
      let individual = auxIndividual { chromosome = map (\ g -> if fst g >= pos1 && fst g <= pos2 then g else (chromosome $ auxIndividual) !! fst g ) 
                                                        (chromosome $ fst parents) }

      -- snd parent contribution
      let individual' i pos posSndParent = if pos == length (chromosome $ snd parents) - 1
            then do return i -- offspring chromosome complete
            else if pos >= pos1 && pos <= pos2
              then individual' i (pos+1) posSndParent
              else if elem (snd $ (chromosome $ snd parents) !! posSndParent) (map (\ g -> snd g) (chromosome $ i))
                then individual' i pos (mod (posSndParent+1) (length (chromosome i)))
                else individual' (modifyChromosome i ((chromosome $ snd parents) !! posSndParent))
                             (pos+1) (mod (posSndParent+1) (length (chromosome i)))
      individual'' <- individual' individual 0 (mod (pos2+1) (length (chromosome $ snd parents)))
     
      return (individual'')
-}
offspring :: Int -> Int -> Float -> Float -> Population -> IO Population
offspring 0 _ _ _ _ = return [] 
offspring n tSize m c p = do -- number of children, tournament size, mutation rate,
                             -- crossover rate, previous population
  parents <- selectParents tSize p
  i <- crossover c parents
  individual <- mutation m i
  rest <- offspring (n-1) tSize m c p

  return (individual : rest)

mutation :: Float -> Individual -> IO Individual
mutation m i = do -- mutation rate, individual
  -- Swap mutation, is an algorithm that will simply swap the genetic information at
  -- two points. Swap mutation works by looping though the genes in the individual’s
  -- chromosome with each gene being considered for mutation determined by the 
  -- mutation rate. If a gene is selected for mutation, another random gene in the
  -- chromosome is picked and then their positions are swapped.

  -- Swap genes according to mutation rate (pos1 with pos2)
  let swapGene i pos1 = case pos1 of
        (-1) -> return i -- chromosome totally scanned
        otehrwise -> do
          gen <- newStdGen
          let r = head $ drop (snd $ chromosome i !! pos1) $ randoms gen :: Float
          if m < r
            then swapGene i (pos1-1)
            else do
	      gen' <- newStdGen
              let pos2 = head $ drop (snd $ chromosome i !! pos1) 
                       $ randomRs (0, length (chromosome i) - 1) gen' :: Int
              let g1 = (pos1, snd $ chromosome i !! pos2)
              let g2 = (pos2, snd $ chromosome i !! pos1)
              let i' = modifyChromosome (modifyChromosome i g2) g1 -- swaping genes 
              swapGene i' (pos1-1)
  swapGene i (length (chromosome i) - 1)

newGeneration :: Int -> Int -> Float -> Float
              -> Cities -> Population -> IO Population
newGeneration e tSize m c cs p = do -- elite, tournament size, mutation rate,
                                    -- crossover rate, cities, previous population
    let pElite = take e $ ordPopulation p 
    p' <- offspring (length p - e) tSize m c p
    return (calcFitness cs $ pElite ++ p')
