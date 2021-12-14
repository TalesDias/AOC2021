#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "parsec"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

{-# language MultiWayIf #-}

module Main(main) where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List

main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"

  case contents of
    Left e -> print e
    Right entries -> do

      let (input, output) = unzip entries

      --print entries 
      print "Part1"
      (print. sum. map (length. uniquePatterns)) output

      print "Part2"
      (print. sum) $ zipWith decodeOutput input output

  return ()

{-
   "Part1"
    514

    "Part2"
    1012272
-}

type Pattern = String
type Entry   = [Pattern]


uniquePatterns :: Entry -> [Pattern]
uniquePatterns = filter ((`elem` [2,3,4,7]). length)

decodeOutput :: Entry -> Entry -> Int
decodeOutput input output = read $ concatMap (show. decodePattern) realOut
  where
    conv    = deductConfig input
    realOut = map conv output

decodePattern :: Pattern -> Int
decodePattern p
  | lenp == 7 = 8
  | lenp == 2 = 1
  | lenp == 3 = 7
  | lenp == 4 = 4
  | lenp == 5 && 'e' `elem`    p = 2
  | lenp == 5 && 'b' `elem`    p = 5
  | lenp == 5                    = 3
  | lenp == 6 && 'd' `notElem` p = 0
  | lenp == 6 && 'c' `notElem` p = 6
  | lenp == 6 && 'e' `notElem` p = 9

  | otherwise = error $ "Invalid Pattern: " ++ p

  where
    lenp = length p


-- Brace yourself, this function is not easy to understand
-- I recommend trying to solve one of the entries by hand
-- before attempting to read the code
deductConfig :: Entry -> (Pattern -> Pattern)
deductConfig xs =
  let
    one    = head $ filter ((==2). length) xs
    seven  = head $ filter ((==3). length) xs
    four   = head $ filter ((==4). length) xs
    sixLen =        filter ((==6). length) xs

    a = head $ seven\\one

    osf  = concat [one, seven, four]
    nine = sixLen!!fst (head$ filter ((==1). length. snd)$ zip [0..]$  map (\\osf) sixLen)
    g    = head $ nine\\osf

    zeroOrSix = delete nine sixLen
    zero      = zeroOrSix!!fst (head$ filter ((==4). length. snd)$ zip [0..]$  map (\\one) zeroOrSix)
    e         = head $ zero\\(osf ++ [g])

    six   = head$ delete zero zeroOrSix
    bAndD = four\\one
    f     = head$ six\\concat [[a], bAndD, [e], [g]]

    c = head$ one\\[f]

    b = head$ zero\\ concat [[a], one, [e], [g]]

    d = head$ bAndD\\[b]

   in map (\x -> 
    if | x==a -> 'a'
       | x==b -> 'b'
       | x==c -> 'c'
       | x==d -> 'd'
       | x==e -> 'e'
       | x==f -> 'f'
       | x==g -> 'g'
       | otherwise -> error $ "invalid letter: " ++ [x]
    )


-- Parsing methods
number :: GenParser Char st Int
number = read <$> many1 digit <?> "number"

parsePattern :: GenParser Char st Pattern
parsePattern = many1 lower <?> "pattern"

parseEntry :: GenParser Char st Entry
parseEntry = sepEndBy1 parsePattern (char ' ')

parseLine :: GenParser Char st (Entry, Entry)
parseLine = (,) <$> parseEntry <* string "| " <*> parseEntry

contentParser :: GenParser Char st [(Entry, Entry)]
contentParser = manyTill (parseLine <* endOfLine) eof

