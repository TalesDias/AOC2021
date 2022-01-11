#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-18.21   
  --package "parsec containers"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}


module Main(main) where

import Text.Parsec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List(group, sort)

main :: IO ()
main = do

  file <- readFile "input.txt"
  let contents = parse contentP  "input.txt" file

  case contents of
    Left e -> print e
    Right (fst, lst, initPoly, rules) -> do

      print "Part1"
      (print. (minimum >>= (.maximum). subtract) . frequency fst lst.  processPolymer 10 rules) initPoly

      print "Part2"
      (print. (minimum >>= (.maximum). subtract) . frequency fst lst.  processPolymer 40 rules) initPoly

  return ()

{-
   "Part1"
    2447

    "Part2"
    3018019237563
-}

-- Data and Type declarations
type PairKey = (Char, Char)
type Polymer = Map PairKey Int
type Rules   = Map PairKey Char

-- Logic
applyRules :: Rules -> Polymer -> Polymer 
applyRules rs = M.foldlWithKey' f M.empty
  where 
    f :: Polymer -> PairKey -> Int -> Polymer
    f py k@(a,b) count 
      | M.notMember k rs = py
      | otherwise        = M.insertWith (+) (a, new) count $
                           M.insertWith (+) (new, b) count py
        where (Just new) = M.lookup k rs
        
processPolymer :: Int -> Rules -> Polymer -> Polymer
processPolymer 0 _  py = py
processPolymer n rs py = processPolymer (n-1) rs (applyRules rs py)

frequency :: Char -> Char -> Polymer -> [Int]
frequency fst lst = halve. M.elems . M.foldlWithKey' f extremes
 where 
   extremes = M.fromList [(fst, 1), (lst,1)]
   halve    = map (`div` 2)
   
   f :: Map Char Int -> PairKey -> Int -> Map Char Int 
   f counter (a, b) qty = M.insertWith (+) a qty $ 
                          M.insertWith (+) b qty counter


-- Parsing methods
type Parser = Parsec String ()

numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"

ruleP :: Parser (PairKey, Char)
ruleP = (,) <$> ((,) <$> upper <*> upper) 
            <*  string " -> " 
            <*> upper

contentP :: Parser (Char, Char, Polymer, Rules)
contentP = do
  poly <- many1 upper
  _    <- endOfLine <* endOfLine
  rs   <- manyTill (ruleP  <* endOfLine) eof
  
  let fst      = head poly
  let lst      = last poly
  let rules    = M.fromList rs
  let initPoly = M.fromListWith (+) $ zip (zip poly (tail poly)) (repeat 1)

  return (fst, lst, initPoly, rules)

