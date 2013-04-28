{-# LANGUAGE TupleSections #-}

module Radix (
      main
    ) where


import Control.Monad
import Data.Char (ord, chr, toUpper)
import Data.List (foldl')
import Data.Maybe
import Numeric (showIntAtBase)
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
	    _ -> mapM_ (uncurry printBases) $ args >>= \arg -> map (,arg) bases


bases :: [Base]
bases = [2, 8, 10, 16]


printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ takeBaseName progName ++ " NUM"


printBases :: (FromBase a) => Base -> a -> IO ()
printBases outBase raw = do
    mapM_ pretty $ processBases outBase raw
    putStrLn ""
    where
	pretty (inBase, res) = putStrLn $ pad inBase ++ " -> " ++ pad outBase ++ ": " ++ res
	pad n = case show n of
	    [d] -> [' ', d]
	    other -> other


processBases :: (FromBase a, ToBase b) => Base -> a -> [(Base, b)]
processBases outBase raw = mapMaybe (\(inBase, n) -> fmap (inBase,) $ toBase outBase n) bns
    where
	bns = mapMaybe (\inBase -> fmap (inBase,) $ fromBase inBase raw) bases







