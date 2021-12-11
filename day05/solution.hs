#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "parsec containers"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main(main) where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M


main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"

  case contents of
    Left e -> print e
    Right segments -> do

      print "Part1"
      (print. length. filter ((2<). snd). M.toList. insertDiagonalSegments) segments

      print "Part2"
      (print. length. filter ((2<). snd). M.toList. insertSegments) segments

  return ()

{-
   "Part1"
    5124

    "Part2"
    19771
-}

type Position = (Int, Int)
type OceanFloor = Map Position Int

data Segment = MkSeg{
  x1 ::Int,
  y1 ::Int,
  x2 ::Int,
  y2 ::Int}
  deriving Show


fromTo :: Int -> Int -> [Int]
fromTo a b
  | a<b       = [a..b]
  | otherwise = reverse [b..a]

expandSegment :: Segment -> [Position]
expandSegment seg@(MkSeg w1 z1 w2 z2)
  | isDiagonal seg = zip (fromTo w1 w2) (fromTo z1 z2)
  | otherwise      = [(x,y) | x <-fromTo w1 w2, y <-fromTo z1 z2]

isDiagonal :: Segment -> Bool
isDiagonal seg = x1 seg /= x2 seg && y1 seg /= y2 seg

insertDiagonalSegments :: [Segment] -> OceanFloor
insertDiagonalSegments = insertSegments. filter (not. isDiagonal)

insertSegments :: [Segment] -> OceanFloor
insertSegments = idl M.empty
  where
    idl :: OceanFloor -> [Segment] -> OceanFloor
    idl ocean []       = ocean
    idl ocean (seg:ss) =
      let newOcean = foldl (\m p -> M.insertWith (+) p 1 m) ocean (expandSegment seg)
      in  idl newOcean ss



-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

segmentParser :: GenParser Char st Segment
segmentParser = MkSeg
  <$> number <* char ','
  <*> number <* string " -> "
  <*> number <* char ','
  <*> number

contentParser :: GenParser Char st [Segment]
contentParser = manyTill (segmentParser <* endOfLine) eof

