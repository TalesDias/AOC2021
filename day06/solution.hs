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


main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"

  case contents of
    Left e -> print e
    Right fishes -> do

      print "Part1"
      (print. getTotalPop. simulateNDays 80. initiatePop) fishes 

      print "Part2"
      (print. getTotalPop. simulateNDays 256. initiatePop) fishes 

  return ()

{-
   "Part1"
    387413

    "Part2"
    1738377086345
-}

data FishPop = MkFishPop {
                         day0 :: Int, day1 :: Int,
                         day2 :: Int, day3 :: Int,
                         day4 :: Int, day5 :: Int,
                         day6 :: Int, day7 :: Int,
                         day8 :: Int
                         }


initiatePop :: [Int] -> FishPop
initiatePop fishes
  = MkFishPop 0
              (length $ filter (==1) fishes)
              (length $ filter (==2) fishes)
              (length $ filter (==3) fishes)
              (length $ filter (==4) fishes)
              (length $ filter (==5) fishes)
              (length $ filter (==6) fishes)
              0
              0

getTotalPop :: FishPop -> Int
getTotalPop (MkFishPop d0 d1 d2 d3 d4 d5 d6 d7 d8)
  = d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8

simulateNextDay :: FishPop -> FishPop
simulateNextDay (MkFishPop d0 d1 d2 d3 d4 d5 d6 d7 d8) 
  = MkFishPop d1 d2 d3 d4 d5 d6 (d7+d0) d8 d0
  
simulateNDays :: Int -> FishPop -> FishPop
simulateNDays days pop 
  = foldl (const. simulateNextDay) pop (replicate days ())


-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

contentParser :: GenParser Char st [Int]
contentParser = sepBy number (char ',') <* endOfLine <* eof

