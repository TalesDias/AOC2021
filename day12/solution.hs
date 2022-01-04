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

import Text.Parsec
import Data.Char(isLower, isUpper)
import Data.List(nub)
import Data.Set(Set)
import qualified Data.Set as S

main :: IO ()
main = do

  file <- readFile "input.txt"
  let contents = runParser contentP [] "input.txt" file

  case contents of
    Left e -> print e
    Right (caverns, connections) -> do

      print "Part1"
      print $ length (findAllPaths caverns connections)

      print "Part2"
      print $ length (findAllPathsv2 caverns connections)

  return ()

{-
   "Part1"
    5254

    "Part2"
    149385
-}

-- Data and Type declarations
data Cavern
    = Start
    | End
    | Big   { cName :: String}
    | Small { cName :: String, cVisited ::Bool}
  deriving (Show, Ord)

instance Eq Cavern where
  Start       == Start       = True
  End         == End         = True
  (Big a)     == (Big b)     = a == b
  (Small a _) == (Small b _) = a == b
  _           == _           = False

visit :: Cavern -> Cavern
visit (Small n _) = Small n True
visit x           = x

visited :: Cavern -> Bool
visited (Small _ True) = True
visited _              = False

modifyFstOccur :: Eq a => (a -> a) -> a -> [a] -> [a]
modifyFstOccur _ _ [] = []
modifyFstOccur f a (x:xs)
  | a == x    = f a :xs
  | otherwise = x : modifyFstOccur f a xs

data Connection =
    Conn { start ::Cavern, end ::Cavern }
  deriving (Show, Eq)

type Path = [Cavern]

-- Logic

findAllPaths :: [Cavern] -> [Connection] -> Set Path
findAllPaths = pathsStartingAt Start False

findAllPathsv2 :: [Cavern] -> [Connection] -> Set Path
findAllPathsv2 = pathsStartingAt Start True

pathsStartingAt :: Cavern -> Bool -> [Cavern] -> [Connection] -> Set Path
pathsStartingAt End            _     cvs cns = S.singleton [End]
pathsStartingAt (Small _ True) False cvs cns = S.empty
pathsStartingAt s              flag  cvs cns = S.map (s:) (S.union withoutFlag withFlag)
  where
    neigh = filter (\c -> isConnected s c cns) cvs
    cands = filter (not. visited) neigh
    ncvs  = modifyFstOccur visit s cvs

    withoutFlag = S.unions $ map (\c -> pathsStartingAt c flag ncvs cns) cands
    withFlag    = if flag
                     then S.unions $ map (\c -> pathsStartingAt c False cvs cns) neigh
                     else S.empty


isConnected :: Cavern -> Cavern -> [Connection] -> Bool
isConnected End cb    cns = False
isConnected ca  Start cns = False
isConnected ca  cb    cns
  | any ((cb==). end) (filter ((ca==). start) cns) = True
  | any ((cb==). start) (filter ((ca==). end) cns) = True
  | otherwise                                      = False


-- Parsing methods
type Parser = Parsec String [Cavern]

str2Cavern :: String -> Cavern
str2Cavern "start" = Start
str2Cavern "end"   = End
str2Cavern ss
  | all isUpper ss = Big ss
  | all isLower ss = Small ss False
  | otherwise      = error $ "Error while parsing Cavern.\n Unexpected pattern: " ++ show ss


numberP :: Parser [Int]
numberP = read <$> many1 digit <?> "digit"

connectionP :: Parser Connection
connectionP = do
  stt <- str2Cavern <$> many letter
  _   <- char '-'
  end <- str2Cavern <$> many letter

  modifyState (stt:)
  modifyState (end:)

  return $ Conn stt end

contentP :: Parser ([Cavern], [Connection])
contentP = do
  conns <- manyTill (connectionP <* endOfLine) eof
  cavs  <- nub <$> getState

  return (cavs, conns)

