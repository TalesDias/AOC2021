#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-18.21   
  --package "parsec containers mtl"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}


module Main(main) where

import Text.Parsec hiding (State)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List(insert, foldl')
import Data.Tuple(swap)
import Control.Monad.State
import Data.Bifunctor


main :: IO ()
main = do

  file <- readFile "input.txt"
  let contents = runParser contentP 0 "input.txt" file

  case contents of
    Left e -> print e
    Right cavern -> do
      
      print "Part1"
      (print. risk. dijkstra) cavern
      
      print "Part2"
      (print. risk. dijkstra. expandMap) cavern

  return ()

{-
   "Part1"
    698

    "Part2"
    3022
-}

-- Data and Type declarations
type Coord  = (Int, Int)
type Cavern = Map Coord (Int, Bool)

data Path = MkPath {
                    path :: [Coord],
                    risk :: Int
                   }
  deriving Show

instance Eq Path where
  (MkPath (x:_) _) == (MkPath (y:_) _) = x == y

instance Ord Path where
  (MkPath _ c1) <= (MkPath _ c2) = c1 <= c2

instance Semigroup Path where
  (MkPath xs c1) <> (MkPath ys c2) = MkPath (xs<>ys) (c1+c2)

startCoord :: Coord
startCoord = (0,0)

singleton :: a -> [a]
singleton = (:[])


-- Logic
dijkstra :: Cavern -> Path
dijkstra cavs = evalState (go []) cavs
  where
    rightbound = fst$ fst$ M.findMax cavs
    lowbound   = fst$ fst$ M.findMax$ M.mapKeys swap cavs
    endCoord   = (lowbound, rightbound) 

    go :: [Path] -> State Cavern Path
    go []     = go $ singleton $ MkPath [startCoord] 0
    go (p:ps) = do
      cv <- get
      let neig = neighbours rightbound lowbound cv (head $ path p)
      let end  = MkPath [endCoord] (fst$ cv! endCoord)
      if endCoord `elem` neig
        then return $ end <> p
        else (do
          let neigPaths = map (\k -> MkPath [k] (fst$ cv!k)) neig
          let newPaths  = map (<>p) neigPaths
          put $ foldl' visit cv neig

          go $ foldl' (flip insert) ps newPaths)

visit :: Cavern -> Coord -> Cavern 
visit = flip $ M.adjust (second (const True))

neighbours :: Int -> Int -> Cavern -> Coord -> [Coord]
neighbours rb lb cv (x,y) 
  = filter (\(z,w) -> z >= 0 && z <= lb &&
                      w >= 0 && w <= rb &&
                      (not. snd. (cv!)) (z,w)) candidates
  where
    candidates = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

expandMap :: Cavern -> Cavern
expandMap cv = M.map (first norm) colOf5 
  where
    sz         = (1+)$ fst$ fst$ M.findMax cv
    norm x     = if x > 9 then x `mod` 9 else x

    rowOf5     = mconcat [M.map (first (+n)) $ M.mapKeys (second (+sz*n)) cv        | n <- [0..4]]
    rowOf5norm = M.map (first norm) rowOf5
    colOf5     = mconcat [M.map (first (+n)) $ M.mapKeys (first (+sz*n)) rowOf5norm | n <- [0..4]]

-- Parsing methods
type Parser = Parsec String Int

numbersP :: Parser [Int]
numbersP = map (read. singleton)  <$> many1 digit <?> "digit"

lineP :: Parser Cavern
lineP = do
  currLine  <- getState
  foldl (addOnMap currLine) M.empty . zip [0..] <$> numbersP

  where
    addOnMap lin m (col, e) = M.insert (lin, col) (e, False) m


contentP :: Parser Cavern
contentP = mconcat <$> manyTill (lineP <* updateState (+1) <* endOfLine) eof

