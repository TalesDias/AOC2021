#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-18.21   
  --package "parsec mtl"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}


module Main(main) where

import Text.Parsec
import Control.Monad.Reader

main :: IO ()
main = do

  file <- readFile "input.txt"
  let contents = parse contentP "input.txt" file

  case contents of
    Left e -> print e
    Right target -> do

      print "Part1"
      (print. sum. enumFromTo 0. runReader maxVelY) target

      print "Part2"
      (print. length. runReader possibleVel) target

  return ()

{-
   "Part1"
    7626

    "Part2"
    2032
-}

-- Data and Type declarations
data Target
  = Target {
    minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int
  }
  deriving Show

data Pos      = MkPos Int Int deriving Show
data Velocity = MkVel Int Int deriving Show

-- Logic
minVelX :: Reader Target Int
minVelX
  = asks 
      ( fst. head
      . filter ((0>). snd)
      . zip [0..]
      . flip (scanl (-)) [1..]
      . minX)


maxVelX :: Reader Target Int
maxVelX = asks maxX


minVelY :: Reader Target Int
minVelY = asks minY


maxVelY :: Reader Target Int
maxVelY = asks (negate. (+1). minY)


possibleVel :: Reader Target [Velocity]
possibleVel = do
  ix <- minVelX
  ax <- maxVelX
  iy <- minVelY
  ay <- maxVelY
  let candidates = [MkVel x y | x <- [ix..ax] , y <- [iy..ay]]

  filterM isValid candidates


isValid :: Velocity -> Reader Target Bool
isValid = asks. go (MkPos 0 0)
  where
  go  :: Pos -> Velocity -> Target -> Bool
  go p@(MkPos px py) v@(MkVel vx vy) tg
    | overshoot p tg = False
    | isInside  p tg = True
    | otherwise      = go npos nvel tg
    where
      npos = MkPos (px+vx)        (py+vy)
      nvel = MkVel (vx-signum vx) (vy-1)


isInside :: Pos -> Target -> Bool
isInside (MkPos x y) tg
  =  x >= minX tg && x <= maxX tg
  && y >= minY tg && y <= maxY tg


overshoot :: Pos -> Target -> Bool
overshoot (MkPos x y) tg
  = x > maxX tg || y < minY tg

-- Parsing methods
type Parser = Parsec String ()


numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"


sigNumberP :: Parser Int
sigNumberP = (char '-' *> (negate <$> numberP)) <|> numberP


contentP :: Parser Target
contentP = do
  _ <- string "target area: x="

  minx <- sigNumberP
  _ <- string ".."

  maxx <- sigNumberP
  _ <- string ", y="

  miny <- sigNumberP
  _ <- string ".."

  maxy <- sigNumberP
  _<- endOfLine *> eof

  return $ Target minx maxx miny maxy


