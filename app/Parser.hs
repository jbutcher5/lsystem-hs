module Parser where

import Text.Parsec

data Rule = Rule { weight :: Float
                 , output :: [Int]
                 }
            deriving (Show, Eq)

parseRules :: Parsec String st [(String, Float, [String])]
parseRules = do
  res <- sepBy parseRule $ char '\n' 
  eof
  return res

parseRule :: Parsec String st (String, Float, [String])
parseRule = do
  (n, w) <- parseCondition
  o <- string " -> " *> sepBy (many1 $ noneOf ['\n', ' ']) (char ' ')
  return (n, w, o)

parseCondition :: Parsec String st (String, Float)
parseCondition = do
  ident <- many1 $ noneOf "{}[]() "
  return (ident, 1.0)
  
