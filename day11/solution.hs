#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "parsec matrix"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}


module Main(main) where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.Matrix
import Data.List(delete)
import qualified Data.Bifunctor as B


main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"

  case contents of
    Left e -> print e
    Right octopuses -> do

      print "Part1"
      (print. fst. runNSteps 100) octopuses

      print "Part2"
      (print. whenFlash) octopuses

  return ()

{-
   "Part1"
    1705

    "Part2"
    265
-}

type Octopuses = Matrix Int
type Pos       = (Int, Int)


relu :: (Ord a, Num a) => a -> a
relu x  | x < 0     = 0
        | otherwise = x


singleton :: a -> [a]
singleton = (:[])


runNSteps :: Int -> Octopuses -> (Int, Octopuses)
runNSteps 0 ocs = (0, ocs)
runNSteps n ocs = B.first (nflash +) nexts
  where
    (nflash, nextOcs) = step ocs
    nexts             = runNSteps (n-1) nextOcs


whenFlash :: Octopuses -> Int
whenFlash = go 0
  where
    go n ocs
      | all (sample ==) ocs = n
      | otherwise           = go (n+1) nextOcs
      where
        sample            = getElem 1 1 ocs
        (nflash, nextOcs) = step ocs


step :: Octopuses -> (Int, Octopuses)
step ocs = B.second (fmap relu) $ go incremented
  where
    incremented = fmap (+1) ocs
    go ocx
        | all (<=9) ocx1 = (n1   , ocx1)
        | otherwise      = (n1+n2, ocx2)
      where
        (n1, ocx1) = partialStep ocx
        (n2, ocx2) = go ocx1


partialStep :: Octopuses -> (Int, Octopuses)
partialStep ocs =
  let ixs = [(x,y) | x <- [1..(nrows ocs)], y <- [1..(ncols ocs)]]
   in foldl upOcto (0, ocs) ixs

  where
    upOcto :: (Int, Octopuses) -> Pos -> (Int, Octopuses)
    upOcto (n, ocs) p
      | ocs ! p <=9 = (n  , ocs)
      | otherwise   = (n+1, blink ocs p)


blink :: Octopuses -> Pos -> Octopuses
blink ocs p = foldl (flip (update (+1))) decr neig
  where
    neig = neighbours ocs p
    decr = setElem (-10) p ocs


neighbours :: Octopuses -> Pos -> [Pos]
neighbours ocs p@(xi, yi) = delete p neig
  where
      neig = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
      minX = if xi-1 > 0          then xi-1 else xi
      maxX = if xi+1 <= nrows ocs then xi+1 else xi
      minY = if yi-1 > 0          then yi-1 else yi
      maxY = if yi+1 <= ncols ocs then yi+1 else yi


update :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
update f (x,y) m = setElem a (x,y) m
  where
    a = f $ getElem x y m


-- Parsing methods
numbers :: GenParser Char st [Int]
numbers = map (read. singleton) <$> many1 digit <?> "many digits"

contentParser :: GenParser Char st Octopuses
contentParser = fromLists <$> manyTill (numbers <* endOfLine) eof

