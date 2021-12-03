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
    Right digits -> do

      print "Part1"
      print $ powerConsumption digits

      print "Part2"
      print $ lifeSupport digits


  return ()

{-
   "Part1"
    2648450

    "Part2"
    2845944 
-}

type Byte = [Int]

powerConsumption :: [Byte] -> Int
powerConsumption = (gamma >>= ((.epsilon) . (*))). toBin. mostCommonBits
  where
    toBin   = map ((flip div 2 . (+1)) . signum)
    gamma   = byte2dec
    epsilon = byte2dec. map (abs. (1-))


lifeSupport :: [Byte] -> Int
lifeSupport = oxygen >>= ((. co2scrubber ) . (*))
  where
    toBin  = map ((flip div 2 . (+1)) . signum)
    oxygen = byte2dec. oxygen'
    oxygen' [c] = c
    oxygen' cs  = target:oxygen' newcs
      where 
        newcs  = map tail. filter ((target==). head) $ cs
        target = (\x-> if x>=0 then 1 else 0). head. mostCommonBits $ cs

    co2scrubber = byte2dec. co2scrubber'
    co2scrubber' [c] = c
    co2scrubber' cs  = target:co2scrubber' newcs
      where 
        newcs  = map tail. filter ((target==). head) $ cs
        target = inv. (\x-> if x>=0 then 1 else 0).  head. mostCommonBits $ cs


inv :: Int -> Int 
inv 0 = 1
inv 1 = 0

byte2dec :: Byte -> Int
byte2dec = sum. zipWith ((*) . (2^)) [0..]. reverse


mostCommonBits :: [Byte] -> [Int]
mostCommonBits = foldl countBits [0,0,0,0,0,0,0,0,0,0,0,0]
  where
    countBits xs = zipWith (+) xs . map (\x -> if x==1 then 1 else -1)




-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

bitParser :: GenParser Char st Int
bitParser = read. (:[]) <$> (try (char '0') <|> char '1')

byteParser :: GenParser Char st Byte
byteParser = count 12 bitParser

contentParser :: GenParser Char st [Byte]
contentParser = manyTill (byteParser <* endOfLine) eof

