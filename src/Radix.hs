{-# LANGUAGE TupleSections #-}

module Radix (
      main
    ) where


import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Numeric
import Base
import System.Environment
import System.FilePath


main :: IO ()
main = do
    args <- getArgs
    if any (`elem` ["-h", "--help"]) args
	then printHelp
	else case args of
	    [] -> printHelp
	    _ -> sequence_ $ mapKnowingLast emit args
    where
	emit NotLast arg = printBases arg >> putStrLn ""
	emit Last arg = printBases arg


data Lastness = NotLast | Last


mapKnowingLast :: (Lastness -> a -> b) -> [a] -> [b]
mapKnowingLast _ [] = []
mapKnowingLast f [x] = [f Last x]
mapKnowingLast f (x:xs) = f NotLast x : mapKnowingLast f xs


newtype InBase = InBase { runInBase :: Base }
    deriving (Eq, Ord)

instance Show InBase where
    show = show . runInBase


newtype OutBase = OutBase { runOutBase :: Base }
    deriving (Eq, Ord)

instance Show OutBase where
    show = show . runOutBase


allBases :: [Base]
allBases = [2 .. 36]


stdBases :: [Base]
stdBases = [2, 8, 10, 16]


bases :: (a -> Base -> Maybe b) -> (Base -> c) -> a -> [(c, b)]
bases f g x = mapMaybe h allBases
    where
	h base = fmap (g base,) $ x `f` base


inBases :: (FromBase a, Integral n) => a -> [(InBase, n)]
inBases = bases fromBase InBase


outBases :: (ToBase a, Integral n) => n -> [(OutBase, a)]
outBases = bases toBase OutBase


genBaseInfo :: (FromBase a, ToBase b, Integral n) => (InBase -> Bool) -> (OutBase -> Bool) -> a -> [(InBase, n, OutBase, b)]
genBaseInfo pIn pOut input = do
    (inBase, num) <- inBases input
    guard $ pIn inBase
    (outBase, output) <- outBases num
    guard $ pOut outBase
    return (inBase, num, outBase, output)


printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ takeBaseName progName ++ " NUM"


printBases :: String -> IO ()
printBases input = sequence_
    $ mapKnowingLast emit
    $ map (unlines . map pretty)
    $ groupBy ((==) `on` first)
    $ filter (not . sameBase)
    $ genBaseInfo pIn pOut input
    where
	pIn = (`elem` map InBase stdBases)
	pOut = (`elem` map OutBase stdBases)
	sameBase (inBase, _, outBase, _) = runInBase inBase == runOutBase outBase
	first (x, _, _, _) = x
	emit lastness = case lastness of
	    NotLast -> putStrLn
	    Last -> putStr


pad :: Int -> String -> String
pad n str = replicate (n - length front) ' ' ++ front ++ end
    where
	(front, end) = span (not . isSpace) str


pretty :: (Integral n, Show n) => (InBase, n, OutBase, String) -> String
pretty (inBase, num, outBase, output) = showBase False inBase ++ show num ++ " -> " ++ showBase True outBase ++ output
    where
	showBase :: (Show a) => Bool -> a -> String
	showBase doPad x = let
	    res = "(" ++ show x ++ ") "
	    in if doPad
		then pad 4 res
		else res



