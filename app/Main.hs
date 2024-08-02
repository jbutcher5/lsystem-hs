module Main where

import Data.HashMap.Strict (HashMap, fromList, toList, (!))
import System.Random (StdGen, mkStdGen, randomIO, split, uniformR)
import Parser
import Text.Parsec (parseTest)

ioSeed = randomIO :: IO Int

rules :: HashMap Int [Rule]
rules = fromList [ (1, [
                       Rule { weight = 0.5
                            , output = [2, 1]
                            },
                       Rule { weight = 0.5
                            , output = [2, 1, 2, 2]
                            }
                       ])
                 , (2, [Rule { weight = 1, output = [1]}])
                 ]

weighRules :: Float -> Float -> [Rule] -> Rule
weighRules _ _ [] = error "No rules specified for key."
weighRules v prob (rule:rs) = if k >= prob then rule else weighRules k prob rs 
  where k = v + weight rule

getRuleOutput :: Int -> Float -> [Int]
getRuleOutput v prob = output $ weighRules 0 prob $ (!) rules v  

updateAll' :: [Int] -> StdGen -> [Int]
updateAll' [] _ = [] 
updateAll' (x:xs) g1 = getRuleOutput x v ++ updateAll' xs g2 
  where (v, g2) = uniformR (0.0, 1.0) g1

simulate :: [Int] -> Int -> StdGen -> [Int]
simulate xs gens rand | gens >= 1 = simulate next_generation (gens - 1) rand
                      | otherwise = xs
  where next_generation = updateAll' xs rand 
  
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
         seed <- ioSeed
         parseTest parseRules "a -> a b\nb -> a a"
         --print $ simulate [1] 10 $ mkStdGen seed
     | otherwise = putStrLn "Weight sum is not 1 for each rule"
