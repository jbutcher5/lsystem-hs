module Main where

import Data.HashMap.Strict (HashMap, fromList, toList, (!))
import System.Random (StdGen, mkStdGen, randomIO, next)

seed = randomIO :: IO Int

data Rule = Rule { weight :: Float
                 , output :: [Int]
                 }

rules :: HashMap Int [Rule]
rules = fromList [ (1, [Rule { weight = 1
                            , output = [2, 1]
                            }])
                 , (2, [Rule { weight = 1, output = [1]}])
                 ]

getRuleOutput :: StdGen -> Int -> [Int]
getRuleOutput rand k = output . head $ (!) rules k


updateAll' :: [Int] -> StdGen -> [Int]
updateAll' (x:xs) gen = getRuleOutput gen x ++ updateAll' xs (gen)

simulate :: [Int] -> Int -> StdGen -> [Int]
simulate xs gens rand | gens >= 1 = simulate next_generation $ gens - 1
                      | otherwise = xs
  where next_generation = concatMap (getRuleOutput rand) xs
  
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
main | checkRules = do
         print $ simulate [1] 2
         x <- seed
         print x
     | otherwise = putStrLn "Weight sum is not 1 for each rule"
