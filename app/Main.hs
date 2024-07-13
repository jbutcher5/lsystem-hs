module Main where

import Data.HashMap.Strict (HashMap, fromList, toList, (!))

data Rule = Rule { weight :: Float
                 , output :: [Int]
                 }

rules :: HashMap Int [Rule]
rules = fromList [ (1, [Rule { weight = 1
                            , output = [2, 1]
                            }])
                 , (2, [Rule { weight = 1, output = [1]}])
                 ]

getRuleOutput :: Int -> [Int]
getRuleOutput k = output . head $ (!) rules k

simulate :: [Int] -> Int -> [Int]
simulate xs gens | gens >= 1 = simulate next $ gens - 1
                 | otherwise = xs
  where next = concatMap getRuleOutput xs
  
-- Check the sum of all weights per rule adds up to 1
checkRules :: Bool
checkRules = checkRules' $ toList rules
checkRules' :: [(Int, [Rule])] -> Bool
checkRules' [] = True
checkRules' ((_, x):xs) = (weightSum x == 1) && checkRules' xs

-- Calculate the sum of the weights
weightSum :: [Rule] -> Float
weightSum = foldr (\rule acc -> weight rule + acc) 0

main :: IO ()
main | checkRules = print $ simulate [1] 2 
     | otherwise = putStrLn "Weight sum is not 1 for each rule"
