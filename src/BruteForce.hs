{-
-- Project: FLP 1.project - Knapsack problem
-- Author: Filip Brna, xbrnaf00
-- Date: March 14th, 2023
-}
module BruteForce where

import Data.List (subsequences, intercalate, maximumBy)
import Data.Char (isDigit)
import Data.Ord (comparing)

-- -i show Knapsack loaded from Stdin or file -------------------------------------------------------------------------
data Item = Item Int Int

data Knapsack = Knapsack {
    maxWeight :: Int,
    minCost :: Int,
    items :: [(Int, Int)]
}

instance Show Item where
    show (Item w c) = "    Item {\n    weight: " ++ show w ++ "\n    cost: " ++ show c ++ "\n    }"

instance Show Knapsack where
    show (Knapsack maxW minC i) =   "Knapsack {\nmaxWeight: " ++ show maxW ++ "\nminCost: " ++ show minC ++ "\nitems: [\n" ++
                                        unlines (map ( show . uncurry Item) i) ++ "]\n}"
--------------------------------------------------------------------------------------------------------------------------

{- 
bruteforce variant 
Generate all combinations, then find most valuable variant with weight bigger then knapsack maxWeight
and cost bigger than minCost, than print solution.
-}  
bruteForce :: Knapsack -> IO ()
bruteForce knapsack =   let bfCombinations = combinations knapsack
                            possibleSolutions = checkBest knapsack bfCombinations
                            (_, sol ) = mostValuable possibleSolutions
                            solution = swapCommaForSpace sol
                        in putStrLn solution

-- swapCommaForSpace take "," and swap for " "
swapCommaForSpace :: Show a => [a] -> String
swapCommaForSpace [] = "False"
swapCommaForSpace xs = "Solution ["++intercalate " " (map show xs)++"]"

-- take list of triples which represent knapsacks (weight,cost,solution) return most valuable one 
mostValuable :: [(a, Int, [b])] -> (Int ,[b])
mostValuable [] = (0, [])
mostValuable lst =  let (_, val, solution) = maximumBy (comparing (\(_, x, _) -> x)) lst
                    in (val, solution)

-- the function finds out which items are in the knapsack and creates the corresponding combination of 1s and 0s, which represent the solution
combinations :: Knapsack -> [([(Int,Int)],[Int])]
combinations knapsack = [(ys, map (\x -> if x `elem` ys then 1 else 0) $ items knapsack) | ys <- subsequences $ items knapsack]


-- return best knapsack combination which meets the conditions
checkBest :: Knapsack -> [([(Int, Int)], b)] -> [(Int, Int, b)]
checkBest knapsack options =    let costs = map (sum . map snd . fst) options
                                    weights = map (sum . map fst . fst) options
                                    solution = map (\(_,s) -> s) options
                                    triples = zip3 weights costs solution
                                in  filter (\(x, y, _) -> x <= maxWeight knapsack && y >= minCost knapsack) triples 

-- from stdin/file remove all nonDigit symbols and that strings transform to int
strToInts :: [String] -> [Int]
strToInts xs = map read $ filter (all isDigit) xs

-- list of integers which represent kanpsack to tuple, lately first tuple would be maxWeigth and minCost and others would be Items
makeTuple :: [a] -> [(a, a)]        
makeTuple [] = []
makeTuple (x:y:ys) = (x,y) : makeTuple ys
makeTuple _ = error "Wrong input: missing weight or cost in some item"