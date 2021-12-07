#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "parsec"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}
{-# LANGUAGE MultiWayIf #-}

module Main(main) where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List
import Data.Tuple


main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"

  case contents of
    Left e -> print e
    Right (numbers, boards) -> do

      print "Part1"
      print $ sum. map fst. filter (not. snd). snd >>= (. fst). (*) $ findWinnerBoard boards numbers

      print "Part2"
      print $ sum. map fst. filter (not. snd). snd >>= (. fst). (*) $ findLastWinnerBoard boards numbers


  return ()

{-
   "Part1"
    32844

    "Part2"
    4920 
-}

type Board = [(Int,Bool)]

findWinnerBoard :: [Board] -> [Int] -> (Int, Board)
findWinnerBoard bs (n:ns) =
  case getWinnerBoard newbs of
    Just b  -> (n, b)
    Nothing -> findWinnerBoard newbs ns

  where newbs = map (markBoard n) bs

findLastWinnerBoard ::[Board] -> [Int] -> (Int, Board)
findLastWinnerBoard bs (n:ns) =
  case getWinnerBoard newbs of
    Just b  ->
      let x = delete b newbs
       in if null x then (n, b) else findLastWinnerBoard x (n:ns)

    Nothing -> findLastWinnerBoard newbs ns

  where newbs = map (markBoard n) bs

markBoard :: Int -> Board -> Board
markBoard n = map mark
  where
    mark (val, True)  = (val, True)
    mark (val, False) = (val, val==n)

getWinnerBoard :: [Board] -> Maybe Board
getWinnerBoard bs = (fmap fst. safeHead. filter snd. zip bs. map (isWinner . map snd)) bs
  where
    isWinner :: [Bool] -> Bool
    isWinner b =
        if | and $ take 5 b                   -> True --Horizontals
           | and $ take 5 $ drop 5  b         -> True
           | and $ take 5 $ drop 10 b         -> True
           | and $ take 5 $ drop 15 b         -> True
           | and $ take 5 $ drop 20 b         -> True

           | and $ [b!!(5*m+0) | m <- [0..4]] -> True --Verticals
           | and $ [b!!(5*m+1) | m <- [0..4]] -> True
           | and $ [b!!(5*m+2) | m <- [0..4]] -> True
           | and $ [b!!(5*m+3) | m <- [0..4]] -> True
           | and $ [b!!(5*m+4) | m <- [0..4]] -> True

           | otherwise                        -> False --No Winner



safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

numbersParser :: GenParser Char st [Int]
numbersParser = manyTill (number <*  optional (char ',')) endOfLine

boardParser :: GenParser Char st Board
boardParser = map (curry swap False). concat
  <$> count 5 (
          count 5 (optional (char ' ')
          *> number
          <* optional (char ' '))
          <* endOfLine
              )


contentParser :: GenParser Char st ([Int], [Board])
contentParser = (,) <$> numbersParser <* endOfLine <*> manyTill (boardParser <* endOfLine) eof

