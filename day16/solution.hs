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

import Text.Parsec hiding (State)
import Control.Monad.State
import Control.Monad(replicateM)


main :: IO ()
main = do

  file <- readFile "input.txt"
  let contents = parse contentP "input.txt" file

  case contents of
    Left e -> print e
    Right packet -> do

      print "Part1"
      (print. sumVersion) packet

      print "Part2"
      (print. calcPacket) packet

  return ()

{-
   "Part1"
    877

    "Part2"
    194435634456
-}

-- Data and Type declarations
data Packet
  = Op {
    version :: Int,
    opId    :: Int,
    subpkt  :: [Packet]
       }
  | Lit {
    version :: Int,
    val     :: Int
        }
  deriving Show

-- Logic

sumVersion :: Packet -> Int
sumVersion (Lit v _)   = v
sumVersion (Op v _ sb) = v + sum (map sumVersion sb)

calcPacket :: Packet -> Int
calcPacket (Lit _ v)    = v
calcPacket (Op _ 0 pks) = sum      $ map calcPacket pks
calcPacket (Op _ 1 pks) = product  $ map calcPacket pks
calcPacket (Op _ 2 pks) = minimum  $ map calcPacket pks
calcPacket (Op _ 3 pks) = maximum  $ map calcPacket pks
calcPacket (Op _ 5 pks) = bool2Num $ f > s
  where [f,s]           = map calcPacket pks

calcPacket (Op _ 6 pks) = bool2Num $ f < s
  where [f,s]           = map calcPacket pks

calcPacket (Op _ 7 pks) = bool2Num $ f == s
  where [f,s]           = map calcPacket pks
  

bool2Num :: Bool -> Int
bool2Num True  = 1
bool2Num False = 0

-- Parsing methods
type Parser = Parsec String ()

hex2Bin :: Char -> [Int]
hex2Bin '0' = [0,0,0,0]
hex2Bin '1' = [0,0,0,1]
hex2Bin '2' = [0,0,1,0]
hex2Bin '3' = [0,0,1,1]
hex2Bin '4' = [0,1,0,0]
hex2Bin '5' = [0,1,0,1]
hex2Bin '6' = [0,1,1,0]
hex2Bin '7' = [0,1,1,1]
hex2Bin '8' = [1,0,0,0]
hex2Bin '9' = [1,0,0,1]
hex2Bin 'A' = [1,0,1,0]
hex2Bin 'B' = [1,0,1,1]
hex2Bin 'C' = [1,1,0,0]
hex2Bin 'D' = [1,1,0,1]
hex2Bin 'E' = [1,1,1,0]
hex2Bin 'F' = [1,1,1,1]
hex2Bin  c  = error $ "hexToBin: no parse for " ++ [c]


bin2Dec :: [Int] -> Int
bin2Dec bs = sum [b*2^p | (b,p) <- zip (reverse bs) [0..]]


takeN :: Int -> State [Int] [Int]
takeN n = do
  ints <- gets (take n)
  modify (drop n)
  return ints


repeatUntil :: (s -> Bool) -> State s a -> State s [a]
repeatUntil p s = do
  r <- gets p
  if r  then pure []
        else liftM2 (:) s (repeatUntil p s)


readLit :: State [Int]  Int
readLit = bin2Dec <$> readGroup
    where
      readGroup = do
        l <- head <$> takeN 1
        if l == 0
           then takeN 4
           else (++) <$> takeN 4 <*> readGroup


readOp :: State [Int] [Packet]
readOp = do
  lenTypeId <- head <$> takeN 1

  if lenTypeId == 0
    then do
      len  <- bin2Dec <$> takeN 15
      bits <- takeN len
      return $ evalState (repeatUntil null dePack) bits

    else do
      qty  <- bin2Dec <$> takeN 11
      replicateM qty dePack


dePack :: State [Int] Packet
dePack = do
  version <- bin2Dec <$> takeN 3
  pckId   <- bin2Dec <$> takeN 3
  if pckId == 4
     then Lit version <$> readLit
     else Op version pckId <$> readOp


packetP :: Parser Packet
packetP = evalState dePack
        . concatMap hex2Bin 
        <$> many1 hexDigit


contentP :: Parser Packet
contentP = packetP <* endOfLine <* eof

