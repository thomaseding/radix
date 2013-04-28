{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Base (
      Base
    , ToBase(..)
    , FromBase(..)
    ) where

import Control.Monad
import Data.Char (ord, chr, toUpper)
import Data.List (foldl')
import Data.Maybe


numToChar :: Int -> Maybe Char
numToChar d
  | 0 <= d && d <= 9 = Just $ chr $ ord '0' + d
  | 10 <= d && d <= 35 = Just $ chr $ ord 'A' + d - 10
  | otherwise = Nothing


charToNum :: Char -> Maybe Int
charToNum c'
  | '0' <= c && c <= '9' = Just $ ord c - ord '0'
  | 'A' <= c && c <= 'Z' = Just $ ord c - ord 'A' + 10
  | otherwise = Nothing
  where
    c = toUpper c'


sum' :: (Integral n)  => [n] -> n
sum' = foldl' (+) 0


type Base = Integer


class ToBase a where
    toBase :: (Integral n) => Base -> n -> Maybe a


instance ToBase String where
    toBase base = fmap fixZero . (mapM numToChar <=< toBase base)
	where
	    fixZero "" = "0"
	    fixZero str = str


instance (Integral) n => ToBase [n] where
    toBase base = Just . reverse . toBase'
	where
	    toBase' 0 = []
	    toBase' n = case n `divMod` fromIntegral base of
		(q, r) -> fromIntegral r : toBase' q


class FromBase a where
    fromBase :: (Integral n) => Base -> a -> Maybe n


instance FromBase String where
    fromBase base = fromBase base <=< mapM (fmap fromIntegral . charToNum)


instance (Integral n) => FromBase [n] where
    fromBase base = fmap (fromIntegral . sum') . sequence . zipWith f (iterate (* fromIntegral base) 1) . reverse
	where
	    f scale n = if n >= fromIntegral base
		then Nothing
		else Just $ n * scale





