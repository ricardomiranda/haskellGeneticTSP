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
calcFitness cs (i:is) = i { fitness = Just (calcTotalDistance (map (findCity cs . snd) (chromosome i))) } 
                     : calcFitness cs is

calcFitnessPopulation :: Population -> Float
calcFitnessPopulation p = 
  sum $ map fit p
    where
      fit x = fromMaybe (error "Clashes unknown, module Population, function calcFitnessPopulation") 
                        (fitness x)

ordPopulation :: Population -> Population
ordPopulation = sort

tournamentSelection :: (RandomGen g) => g -> Int -> Population -> (Individual, g)
tournamentSelection gen n p = -- tournament size, previous population
  -- Tournament selection selects its parents by running a series of "tournaments".
  -- First, individuals are randomly selected from the population and entered into a
  -- tournament. Next, these individuals can be thought to compete with each other
  -- by comparing their fitness values, then choosing the individual with the highest
  -- fitness for the parent.
  let (gs, gen') = randList gen n p in
  (head $ ordPopulation gs, gen')

selectParents :: (RandomGen g) => g -> Int -> Population 
                               -> ((Individual, Individual), g)
selectParents g n p = -- tournament size, previous population
  let (parent1, g') = tournamentSelection g n p in
  let (parent2, g'') = tournamentSelection g' n p in
  ((parent1, parent2), g'')

crossover :: (RandomGen g) => g -> Float -> (Individual, Individual) -> Cities
                           -> Individual
crossover gen c parents s = -- crossover rate, (first parent, second parent), cities
  -- Single point crossover is an alternative crossover method to the uniform cross-
  -- over method we implemented previously. Single point crossover is a very simple
  -- crossover method in which a single position in the genome is chosen at random
  -- to define which genes come from which parent. The genetic information before
  -- the crossover position comes from parent1, while the genetic information, after the
  -- position, comes from parent2.
  let r = head $ take 1 $ randoms gen :: Float in
  let (_, gen') = next gen in 
  if c < r 
    then fst parents
    else 
      let (auxIndividual, gen'') = createIndividual gen' (length $ chromosome (fst parents)) in

      -- random swap point
      let (pos, _) = randomR (0, length (chromosome (fst parents)) - 1) gen'' in
      let mixGene i = if i < pos 
          then (i, snd $ chromosome (fst parents) !! i)
          else (i, snd $ chromosome (snd parents) !! i) in

      newIndividual (map (mixGene . fst ) $ chromosome auxIndividual) Nothing
{-
offspring :: (RandomGen g) => g -> Int -> Int -> Float -> Float -> School
                           -> Population -> Population
offspring _ 0 _ _ _ _ _ = [] 
offspring g n tSize m c s p = -- elite, tournament size, mutation rate, crossover rate, 
                              -- school, previous population
  let (parents, g') = selectParents g tSize p in
  let individual = mutation g' m s $ crossover g' c parents s in

  let (_, g'') = next g in
  individual : offspring g'' (n-1) tSize m c s p

mutation :: (RandomGen g) => g -> Float -> School -> Chromosome -> Chromosome
mutation gen m s i = -- mutation rate, school, individual
  -- In uniform crossover, genes are selected at random from an existing and valid parent.
  -- The parent might not be the fittest individual in the population, but at least it's
  -- valid.
  -- Here we create a new random but valid individual and essentially run uniform crossover
  -- to achieve mutation! Afterward we select genes from the random Individual to copy into 
  -- the Individual to be mutated. This technique is called uniform mutation, and makes
  -- sure that all of our mutated individuals are fully valid, never selecting a gene that 
  -- doesn't make sense.

  -- Create random individual to swap genes with
  let (c, gen') = createChromosome gen s in
  let randomIndividual = newChromosome (zip [ 0 .. ] c) Nothing in

  -- Swap genes according to mutation rate
  let swapGene gen g =
        let r = head $ drop (snd g) $ randoms gen :: Float in
        if m < r
          then g
	  else (fst g, snd $ genes randomIndividual !! fst g) in

  newChromosome (map (swapGene gen') $ genes i) Nothing 

newGeneration :: (RandomGen g) => g -> Int -> Int -> Float -> Float
                               -> School -> Population -> IO (Population, g)
newGeneration g e tSize m c s p = do -- elite, tournament size, mutation rate,
                                     -- crossover rate, school, previous population
  let pElite = take e $ ordPopulation p
  let p' = offspring g (length p - e) tSize m c s p
  let (_, g') = next g
  return (calcFitness s $ pElite ++ p', g')
  -}
