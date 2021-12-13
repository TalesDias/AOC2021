#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "parsec"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main(main) where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List(sort)

main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"

  case contents of
    Left e -> print e
    Right crabs -> do
      
      print "Part1"
      (print. linearSearchMin fuelSpentv1) crabs

      print "Part2"
      (print. linearSearchMin fuelSpentv2) crabs

  return ()

{-
   "Part1"
    336120

    "Part2"
    96864235
-}

type Position = Int
type Fuel     = Int


fuelSpentv1 :: Position -> [Position] -> Fuel
fuelSpentv1 x = sum. map (abs. (x-)) 

fuelSpentv2 :: Position -> [Position] -> Fuel
fuelSpentv2 x = sum. map (calc. abs. (x-))
  where 
    calc x = sum [0..x]


linearSearchMin :: (Position -> [Position] -> Fuel) -> [Position] -> Fuel
linearSearchMin calcFuel xs = go 0
    where
      go i = if calcFuel i xs < calcFuel (i+1) xs
              then calcFuel i xs
              else go (i+1)
              


-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

contentParser :: GenParser Char st [Position]
contentParser = sepBy number (char ',') <* endOfLine <* eof

