#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "parsec containers split"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}


module Main(main) where

import Text.Parsec
import Data.Set(Set)
import qualified Data.Set as S
import Data.List.Split(chunksOf)

main :: IO ()
main = do

  file <- readFile "input.txt"
  let contents = parse contentP  "input.txt" file

  case contents of
    Left e -> print e
    Right (paper, folds) -> do

      print "Part1"
      let firstFold = head folds
      (print. S.size. foldPaper firstFold) paper

      print "Part2"
      (printPaper. foldl (flip foldPaper) paper) folds

  return ()

{-
   "Part1"
    664

    "Part2"
    ####.####...##.#..#.####.#....###..#...
    #....#.......#.#.#.....#.#....#..#.#...
    ###..###.....#.##.....#..#....###..#...
    #....#.......#.#.#...#...#....#..#.#...
    #....#....#..#.#.#..#....#....#..#.#...
    ####.#.....##..#..#.####.####.###..####
-}

-- Data and Type declarations
type Point = (Int, Int)
type Paper = Set Point

data FoldType = Horizontal | Vertical
  deriving Show

data Fold = Fold FoldType Int
  deriving Show

-- Logic
foldPaper :: Fold -> Paper -> Paper
foldPaper = S.map. movePoint

movePoint :: Fold -> Point -> Point
movePoint (Fold Horizontal h) (x, y)
  | h < y     = (x, (2*h)-y)
  | otherwise = (x, y)

movePoint (Fold Vertical v) (x, y)
  | v < x     = (2*v-x, y)
  | otherwise = (x, y)

printPaper :: Paper -> IO ()
printPaper pp = writeFile "./paper.txt" $ concat lsStr
  where
    lsPos = [(x,y) | y <- [0..(S.findMax $ S.map snd pp)]
                , x <- [0..(S.findMax $ S.map fst pp)]]

    lsBool = map (`S.member` pp) lsPos
    lsChar = map (\x -> if x then '#' else '.') lsBool
    lsStr  = map (\l -> l ++ ['\n']) $ chunksOf ((+) 1 $ S.findMax $ S.map fst pp) lsChar


-- Parsing methods
type Parser = Parsec String ()

numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"


pointP :: Parser Point
pointP = (,) <$> numberP <* char ',' <*> numberP


foldP :: Parser Fold
foldP = do
  _ <- string "fold along "
  c <- oneOf "xy"
  _ <- char '='

  let t = if c == 'y' then Horizontal else Vertical

  Fold t <$> numberP


contentP :: Parser (Paper, [Fold])
contentP = do
  points <- many1 (pointP <* endOfLine)
  _      <- endOfLine
  folds  <- manyTill (foldP  <* endOfLine) eof

  return (S.fromList points, folds)

