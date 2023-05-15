{-
-- Project: FLP 1.project - Knapsack
-- Author: Filip Brna, xbrnaf00
-- Date: March 14th, 2023
-}


import ParseArg (getFilename, getMode, getCont)
import BruteForce (bruteForce, makeTuple, strToInts, Knapsack(..))
import GeneticAlg (geneticAlgo, numOfGenerations)
import System.Environment
import System.Random

main :: IO ()
main =  do
        args <- getArgs  
        let filename = getFilename args
            mode =  getMode args

        case (filename, mode) of
            (Nothing, _) -> error "Wrong input: file name is missing, help: ./flp22-fun -i|-b|-o [filename]"
            (_, Nothing) -> error "Wrong input: un-supported mode selected, help: ./flp22-fun -i|-b|-o [filename]"
            (_, _) -> return() 


        contents <- getCont filename

        let intsFromInput = makeTuple $ strToInts $ words contents
            maxWeight' = fst $ head intsFromInput
            minCost' = snd $ head intsFromInput
            items' = tail intsFromInput
            knapsack = Knapsack maxWeight' minCost' items'

        gen <- getStdGen

        if mode == Just ("-b") 
            then bruteForce knapsack
            else if mode == Just ("-o")
                    then geneticAlgo knapsack numOfGenerations gen
                    else print knapsack                                                         