#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-17.5   
  --package "parsec containers"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}


module Main(main) where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec hiding (Line)
import Data.List(sort)

main :: IO ()
main = do

  contents <- parseFromFile contentParser "input.txt"

  case contents of
    Left e -> print e
    Right lns -> do

      print "Part1"
      (print. totalCheckerScore. map checkLine) lns

      print "Part2"
      (print. totalCompleteScore. map checkLine) lns

  return ()

{-
   "Part1"
    464991

    "Part2"
    3662008566
-}

-- types and datas
type Line = [Char]
invalidCharError c = error ("InvalidCharacter: " ++ [c])

data Status = Valid | Corrupted | Missing
  deriving Show

type StatusValue :: Status -> *
data StatusValue s where
  ValidS     :: StatusValue Valid
  CorruptedS :: Char -> StatusValue Corrupted
  MissingS   :: [Char] -> StatusValue Missing
deriving instance Show (StatusValue s)


data ExStatus where
  ExStatus :: StatusValue a -> ExStatus
deriving instance Show ExStatus


-- actual logic
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

checkLine :: forall a. Line -> ExStatus
checkLine []     = ExStatus ValidS
checkLine [c]    = ExStatus $ MissingS [getClosing c]
checkLine (c:cs) = go cs [c]
  where
    go [ ] [ ] = ExStatus ValidS
    go [ ]  xs = ExStatus $ MissingS $ map getClosing xs
    go xs  [ ] = checkLine xs
    go (b:bs) (s:stck)
      | isOpenBracket b = go bs (b:s:stck)
      | match s b       = go bs stck
      | otherwise       = ExStatus $ CorruptedS b


isOpenBracket :: Char -> Bool
isOpenBracket = flip elem "([{<"

match :: Char -> Char -> Bool
match '(' ')' = True
match '[' ']' = True
match '{' '}' = True
match '<' '>' = True
match  _   _  = False

totalCheckerScore :: [ExStatus] -> Int
totalCheckerScore [ ]    = 0
totalCheckerScore (s:ss) = calc s + totalCheckerScore ss
  where
    calc :: ExStatus -> Int
    calc (ExStatus ValidS)           = 0
    calc (ExStatus (MissingS _))     = 0
    calc (ExStatus c@(CorruptedS _)) = checkerScore c


totalCompleteScore :: [ExStatus] -> Int
totalCompleteScore ss = ((!!) <*> (flip div 2. length)) $  sort points
  where
    points = map completeScore $ catMissing ss

    catMissing :: [ExStatus] -> [StatusValue Missing]
    catMissing []                             = []
    catMissing ((ExStatus m@(MissingS _)):xs) = m: catMissing xs
    catMissing (_:xs)                         = catMissing xs

checkerScore :: StatusValue Corrupted -> Int
checkerScore (CorruptedS ')') = 3
checkerScore (CorruptedS ']') = 57
checkerScore (CorruptedS '}') = 1197
checkerScore (CorruptedS '>') = 25137
checkerScore (CorruptedS  c ) = invalidCharError c

completeScore :: StatusValue Missing -> Int
completeScore (MissingS xs)
  = foldl (\acc c -> acc*5+c) 0 scores
  where
    scores = map table xs
    table ')' = 1
    table ']' = 2
    table '}' = 3
    table '>' = 4
    table  c  = invalidCharError c


getClosing :: Char -> Char
getClosing '(' = ')'
getClosing '[' = ']'
getClosing '{' = '}'
getClosing '<' = '>'
getClosing  c  = invalidCharError c



-- Parsing methods
parseLine :: GenParser Char st [Char]
parseLine = many (oneOf "(){}[]<>")

contentParser :: forall a st. GenParser Char st [[Char]]
contentParser = manyTill (parseLine <* endOfLine) eof

