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
    Right directions -> do
      
      print "Part1"
      print $ (. hPos) . (*) =<< depth  $ foldl move (Pos 0 0 0) directions

      print "Part2"
      print $ (. hPos) . (*) =<< depth  $ foldl move2 (Pos 0 0 0) directions


  return ()

{-
  "Part1"
    1694130

  "Part2"
    1698850445 
-}

data Dir = Forward Int | Down Int | Up Int 
  deriving Show

data Pos = Pos { depth ::Int, hPos ::Int, aim ::Int} 
  deriving Show

modDepthBy :: Int -> Pos -> Pos
modDepthBy v (Pos d h a) = Pos (v+d) h a

modHPosBy :: Int -> Pos -> Pos
modHPosBy v (Pos d h a) = Pos d (v+h) a

modAimBy :: Int -> Pos -> Pos
modAimBy v (Pos d h a) = Pos d h (v+a)

move :: Pos -> Dir -> Pos
move p dir = case dir of
                      Forward x -> modHPosBy  x    p 
                      Down x    -> modDepthBy x    p 
                      Up x      -> modDepthBy (-x) p

move2 :: Pos -> Dir -> Pos
move2 p@(Pos _ _ a) dir = case dir of
                      Forward x -> modDepthBy (x*a) $ modHPosBy x p
                      Down x    -> modAimBy x    p 
                      Up x      -> modAimBy (-x) p



-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

dirParser :: GenParser Char st Dir
dirParser = try (Up      <$> (string "up "      *> number))
        <|> try (Down    <$> (string "down "    *> number))
        <|> try (Forward <$> (string "forward " *> number))

contentParser :: GenParser Char st [Dir]
contentParser = manyTill (dirParser <* endOfLine) eof
