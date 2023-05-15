{-
-- Project: FLP 1.project - Knapsack problem
-- Author: Filip Brna, xbrnaf00
-- Date: March 14th, 2023
-}
module GeneticAlg where

import BruteForce (combinations, swapCommaForSpace, mostValuable, Knapsack(..))
import System.Random

-- how many individuals have to be seleceted from population to tournament
numToSelect :: Int
numToSelect = 64 -- 64

populationSize :: Int
populationSize = 500 -- 2000

numOfGenerations :: Int
numOfGenerations = 2000 -- 500

reproductionRate :: Float
reproductionRate = 0.3 -- 0.3

mutationRate :: Float
mutationRate = 0.15 -- 0.1


{- 
genetic algorithm variant 
Generate all combinations, then randomly select few of them, value their fitness and start to generate new generations
after that print most valuable one which meets the conditions, otherwise False
-}  
geneticAlgo :: Knapsack -> Int -> StdGen -> IO ()
geneticAlgo knapsack numOfGene gen = 
                                    let individuals = combinations knapsack
                                        numOfUnique = length individuals
                                        (population, gen') = selection individuals populationSize numOfUnique gen
                                        fitness = fitnessFunction knapsack population
                                        (maxGenCost, solution) = newGens fitness numOfGene knapsack gen' (0,[])
                                    in  if maxGenCost >= minCost knapsack
                                        then putStrLn $ swapCommaForSpace solution
                                        else putStrLn "False"
{- 
newGens include process of selection -> tournament -> reproduction or crossover and mutation,
also find most valuable individual from all generations
-}
newGens :: (Ord a, Num a) => [(Int, Int, [Int])] -> a -> Knapsack -> StdGen -> (Int, [Int]) -> (Int, [Int])
newGens fitOfGen remainGenerations knapsack gen (actualMax, actualSolution) =
                                    let (selected, _) = selection fitOfGen numToSelect populationSize gen
                                        parents = tournament selected
                                        (genFromTournaments, gens) = genOfParents fitOfGen parents [] (populationSize - 2) gen
                                        beforeCrosMut = map (\(_, _, solution) -> solution) $ genFromTournaments
                                        (afterCrosMut, generator) = crossOrRepro beforeCrosMut gens
                                        valueNewGen = whatsIn knapsack afterCrosMut
                                        fitNewGen = fitnessFunction knapsack valueNewGen
                                        (genMax, newSolution) = mostValuable fitNewGen
                                    in  if remainGenerations > 0
                                        then    if genMax > actualMax
                                                then newGens (fitnessFunction knapsack valueNewGen) (remainGenerations-1) knapsack generator (genMax, newSolution)
                                                else newGens (fitnessFunction knapsack valueNewGen) (remainGenerations-1) knapsack generator (actualMax, actualSolution)
                                        else    (genMax, actualSolution)


-- "put into backpack" items based on the solution from the input list of combinations
whatsIn  :: Knapsack -> [[Int]] -> [([(Int, Int)], [Int])]
whatsIn knapsack selections =   map (\s -> (selectedItems knapsack s, s)) selections
                                where selectedItems ks s = [item | (item, 1) <- zip (items ks) s]

{-
crossOrRepro decides whether the parents (winners of the tournament) will continue into the next generation
or whether their children will be created with crossover and mutation
-}
crossOrRepro :: (Eq a, Num a) => [[a]] -> StdGen -> ([[a]], StdGen)
crossOrRepro (child1:child2:childs) gen =   let (randomFloat, gen') = randomR (0,1) gen :: (Float, StdGen)
                                            in  if      randomFloat <= reproductionRate
                                                then    let (crossMutChilds, finalGen) = crossOrRepro childs gen'
                                                        in  (child1:child2 : crossMutChilds, finalGen)         
                                                else    let (crossMutChilds', newGene) = crossover [child1,child2] gen'
                                                            (others, finalGen) = crossOrRepro childs newGene
                                                        in  (crossMutChilds' ++ others, finalGen)    
crossOrRepro _ gen = ([],gen)  

-- crossover create children on the basis of parents and also call mutation
crossover :: (Eq a, Num a) => [[a]] -> StdGen -> ([[a]], StdGen)
crossover (child1:child2:childs) gen =  
                                    let half = (length child1) `div` 2
                                        newchild1 = (take half child1) ++ (drop half child2) 
                                        newchild2 = (take half child2) ++ (drop half child1) 
                                        (mutChild1, gen') = mutation newchild1 gen
                                        (mutChild2, gen'') = mutation newchild2 gen'
                                        (mutChilds, finalGen) = crossover childs gen''
                                    in  (mutChild1:mutChild2:mutChilds, finalGen) 
crossover _ gen = ([], gen)

-- mutation flip randomly some bits of an individual
mutation :: (Eq a, Num a) => [a] -> StdGen -> ([a], StdGen)
mutation [] gen = ([], gen) 
mutation (x:xs) gen =    let (randomFlip, gen') = randomR (0,1) gen :: (Float, StdGen)
                    in  if randomFlip <= mutationRate
                        then    if x == 1
                                then    let (remain, finalGen) = mutation xs gen'
                                        in  (0: remain, finalGen)
                                else    let (remain, finalGen) = mutation xs gen'
                                        in  (1: remain, finalGen)
                        else    let (remain, finalGen) = mutation xs gen'
                                in  (x: remain, finalGen)


-- selection pick randomly few individuals from population 
selection :: (Eq t, Num t) => [a] -> t -> Int -> StdGen -> ([a], StdGen)
selection _ 0 _ gen = ([], gen) 
selection population remainSelect listLen gen = let (randomIndex, gen') = randomR (0,(listLen-1)) gen :: (Int, StdGen)
                                                    (selected, finalGen) = selection population (remainSelect - 1) listLen gen'
                                                in ((population !! randomIndex) : selected, finalGen)

-- fight take first two individuals from given list and that one with higher fitness win and continue to next fight or or becomes a parent
fight :: Ord b => [(a, b, c)] -> [(a, b, c)]
fight ((weigthFst, fitnessFst, solutionFst):(weigthSnd, fitnessSnd, solutionSnd):others)  = if fitnessFst > fitnessSnd
                                                                                            then (weigthFst, fitnessFst, solutionFst):(fight others)
                                                                                            else (weigthSnd, fitnessSnd, solutionSnd):(fight others)       
fight _ = []

-- fight selected individuals until just parents remain
tournament :: Ord b => [(a, b, c)] -> [(a, b, c)]
tournament selected
    | length selected == 2 = selected
    | otherwise = tournament (fight selected)

-- create population of tournament winners (parents which won thier tournament)   
genOfParents :: (Num t1, Ord b, Eq t1) => [(a, b, c)] -> [(a, b, c)] -> t2 -> t1 -> StdGen -> ([(a, b, c)], StdGen)
genOfParents _ x _ 0 gen = (x,gen)
genOfParents fitness (parent1:parent2:_) newG remain gen =
                                        let (selected, gen') = selection fitness numToSelect populationSize gen
                                            (newParents, finalGen) = genOfParents fitness (tournament selected) newG (remain - 2) gen'
                                        in (parent1:parent2:newParents, finalGen)
genOfParents _ _ _ _ gen = ([],gen)

-- value fitness of given knapsack combination based on the items in it
fitnessFunction :: Knapsack -> [([(Int, Int)], b)] -> [(Int, Int, b)]
fitnessFunction knapsack population =   let fitness = map (\(itemInfo, _) -> if totalWeight itemInfo > maxWeight knapsack then 0 else sum (map snd itemInfo)) population
                                            weights = map (totalWeight . fst) population
                                            solution = map (\(_,s) -> s) population
                                            triples = zip3 weights fitness solution
                                            totalWeight itemInfo = sum $ map fst itemInfo
                                        in triples       