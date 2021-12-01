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
import Control.Monad

main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"
  
  case contents of
    Left e -> print e
    Right nums -> do

      print "Part1"
      (print. length . filter id. (tail >>= zipWith (>))) nums

      print "Part2"
      let sumNums = (tail >>= (tail >>= zipWith3 sum3)) nums
      (print. length . filter id. (tail >>= zipWith (>))) sumNums
      

  return ()

{-
  "Part1"
    1766

  "Part2"
    1797 
-}

sum3 x y z = x + y + z

-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

contentParser :: GenParser Char st [Int]
contentParser = manyTill (number <* endOfLine) eof
