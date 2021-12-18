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
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as S
-- It's not necessary to use Set here, but idk, i'm practicing
import Data.List(sort)

main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"

  case contents of
    Left e -> print e
    Right heights -> do

      print "Part1"
      (print. sum. map (+1). findLowPoints) heights

      print "Part2"
      (print. product. take 3. reverse. sort. map length. findBasins) heights

  return ()

{-
   "Part1"
    478

    "Part2"
    1327014
-}

type Index     = (Int, Int)
type HeightMap = [[Int]]


singleton :: a -> [a]
singleton x = [x]

(!?) :: [[a]] -> Index -> Maybe a
(!?) ss (x,y)
  | x < 0 || y <0       = Nothing
  | y >= length ss      = Nothing
  | x >= length (ss!!y) = Nothing
  | otherwise           = Just (ss !!y !!x)

infixr !?


findLowPoints :: HeightMap -> [Int]
findLowPoints hm = mapMaybe (hm !?) ids
  where
    ids = allLowPoints hm

findBasins :: HeightMap -> [[Index]]
findBasins hm = map (exploreBasin hm) (allLowPoints hm)

exploreBasin :: HeightMap -> Index -> [Index]
exploreBasin hm ii = S.toList$ go [ii] S.empty
  where
    go :: [Index] -> Set Index -> Set Index
    go []       bs = bs
    go (ix:ixs) bs = if ix `S.member` bs
                        then go ixs bs
                        else go (ixs++cands) (S.insert ix bs)
      where
        neigh = neighbours ix hm
        cands = filter (maybe False (9 /=) . (hm!?)) neigh


allLowPoints :: HeightMap -> [Index]
allLowPoints hm = filter (`isMinimum` hm) $ [(x,y)
                                          | y <- [0..(length hm -1)]
                                          , x <- [0..(length (head hm) -1)]]

isMinimum :: Index -> HeightMap -> Bool
isMinimum ii@(ix, iy) hm = all (fromMaybe True) comps
  where
    elem  = hm !!iy !!ix
    neigh = neighbours ii hm
    comps = (fmap. fmap) (elem<) (map (hm!?) neigh)

neighbours :: Index -> HeightMap -> [Index]
neighbours (ix, iy) hm = [up, down, left, right]
  where
    up    = (ix+1,iy)
    down  = (ix-1,iy)
    left  = (ix,iy-1)
    right = (ix,iy+1)



-- Parsing methods
parseLine :: GenParser Char st [Int]
parseLine = map (read. singleton) <$> many1 digit

contentParser :: GenParser Char st HeightMap
contentParser = manyTill (parseLine <* endOfLine) eof

