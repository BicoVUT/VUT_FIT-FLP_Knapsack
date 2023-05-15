{-
-- Project: FLP 1.project - Knapsack problem
-- Author: Filip Brna, xbrnaf00
-- Date: March 14th, 2023
-} 
module ParseArg where
import Data.Maybe (fromJust)

getFilename :: [[Char]] -> Maybe [Char]
getFilename args = if (length args > 2) || ( (length args == 2) && (last args) `elem` ["-i", "-b", "-o"])
                           then Nothing --Wrong input
                           else if (length args == 1)
                                    then Just ("Stdin") -- Stdin
                                    else Just (last args) -- file name

getMode ::  [[Char]] -> Maybe [Char]
getMode args
   | "-i" `elem` args = Just "-i"
   | "-b" `elem` args = Just "-b"
   | "-o" `elem` args = Just "-o"
   | otherwise        = Nothing

getCont :: Maybe [Char] -> IO String
getCont filename =   if filename == Just ("Stdin") 
                        then getContents
                        else readFile (fromJust filename)