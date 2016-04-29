module Main where

import Data.Maybe
import System.Environment
import System.Exit
import System.Random
import Graphics.EasyPlot
import Init
import Cities
import City
import Individual
import Population

helpMessage :: IO ()
helpMessage = do
  putStrLn "To run this program type:"	
  putStrLn "geneticClasses, max number of iterations, number of cities, size of the population, mutation rate, crossover rate, number of elite elements that are preserved butween generations, tournament size"
  putStrLn "geneticClasses Int Int Int Float Float Int Int"
  
readn :: String -> Int
readn s = read s :: Int

main :: IO ()
main = do 
  args <- getArgs
  if head args == "--help" || null args
    then helpMessage
    else case length args of
      7 -> do
        let iter = read $ head args :: Int-- max number of generations
        let nbrCities = read (args !! 1) :: Int -- number of cities
        let size = read (args !! 2) :: Int-- population size
        let mutationRate = read (args !! 3) :: Float
        let crossoverRate = read (args !! 4) :: Float
        let elite = read (args !! 5) :: Int -- elitism allows the fittest individual,
                                            -- or individuals, to be added unaltered 
                                            -- to the next generations population. 
                                            -- Size of elit population
        let tournamentSize = read (args !! 6) :: Int
  
        cities <- newMap nbrCities
	p <- createPopulation size nbrCities
        let population = calcFitness cities p
  
        fitness <- loop 0 iter elite tournamentSize mutationRate 
                        crossoverRate cities population

        print $ head fitness
        print $ last fitness
        printGraphic fitness

        print "End of program, aditional output in graphic Fitness.png" 

      _ -> do
        putStrLn "Invalid input, invalid number of arguments"
        putStrLn "Type --help"

loop :: Int -> Int -> Int -> Int -> Float -> Float 
     -> Cities -> Population -> IO [ (Int, Float, Float, Int) ]
loop _ 0 _ _ _ _ cs p = do
  printSolution (head $ ordPopulation p) cs
  return []
loop n iter e tSize m c cs p = do -- curent iteration, max iterations,
                                  -- elite, tournament size, mutation rate, 
                                  -- crossover rate, cities, 
                                  -- previous population 
  p' <- newGeneration e tSize m c cs p
  let f = fromMaybe (error "Fitness not available, module Main, function loop") 
                    (fitness (head $ ordPopulation p'))

  let result = (n, f, calcFitnessPopulation p', length p')
  rest <- loop (n+1) (iter-1) e tSize m c cs p' 

  return (result : rest ) 

printGraphic :: [ (Int, Float, Float, Int) ] -> IO Bool
printGraphic fitness =
  plot (PNG "Fitness.png") 
    [ Data2D [ Title "Population Fitness", Style Lines, Color Red ] []  
      $ map (\ (i, _, fp, _) -> (fromIntegral i, fp) ) fitness
    , Data2D [ Title "Best Individual Fitness", Style Lines, Color Blue ] []
      $ map (\ (i, f, _, _) -> (fromIntegral i, f) ) fitness
    , Data2D [ Title "Total Population", Style Lines, Color Green ] []
      $ map (\ (i, _, _, size) -> (fromIntegral i, fromIntegral size) ) fitness ]

printSolution :: Individual -> Cities -> IO ()
printSolution individual cities = do
  let outputCities = map (\ city -> "City " ++ show (City.id city) ++ ", @ " 
                                            ++ show (pos city)) cities
  let outputTSP = map (\ g -> "Visit: " ++ show (fst g) ++ ", city " 
                                        ++ show (snd g)) 
                      (chromosome individual)
  putStrLn "--------------------"
  putStrLn "Cities are:"
  mapM_ print outputCities
  putStrLn "--------------------"
  putStrLn ("Total fitness: " ++ show (Just (fitness individual)))
  putStrLn "--------------------"
  putStrLn "Best solution is:"
  mapM_ print outputTSP
  putStrLn "--------------------"

