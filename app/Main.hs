module Main where

import Data.HashMap.Strict (HashMap, fromList, toList)

data Rule = Rule { weight :: Float
                 , output :: [Int]
                 }

rules :: HashMap Int [Rule]
rules = fromList [ (1, [Rule { weight = 1
                            , output = [2, 1]
                            }])
                 , (2, [Rule { weight = 1, output = [1]}])
                 ]

checkRules :: Bool
checkRules = checkRules' $ toList rules

checkRules' :: [(Int, [Rule])] -> Bool
checkRules' [] = True
checkRules' ((_, x):xs) = (weightSum x == 1) && checkRules' xs

weightSum :: [Rule] -> Float
weightSum = foldr (\rule acc -> weight rule + acc) 0

main :: IO ()
main = if checkRules then putStrLn "Hello, Haskell!" else putStrLn "Weights do not add up"
