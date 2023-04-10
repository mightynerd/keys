module Lib where

import Data.List (elemIndex, foldl')
import Data.Maybe (fromMaybe)

-- Join a list of string with `sep` as the separator
joins :: [String] -> String -> String
joins [s] sep = s
joins (s : ss) sep = s ++ sep ++ joins ss sep

-- Regular `map` but with the index supplied as a second argument to `f`
mapi :: (a -> Int -> b) -> [a] -> [b]
mapi f as = mapi' f 0 as
    where
        mapi' :: (a -> Int -> b) -> Int -> [a] -> [b]
        mapi' _ _ [] = []
        mapi' f i (a : as) = f a i :  mapi' f (i+1) as

-- Parse hexadecimal string
parseHex :: String -> Int
parseHex = foldl' f 0 where
    f n c = 16*n + fromMaybe 0 (elemIndex c "0123456789ABCDEF")

find :: [a] -> (a -> Bool) -> Maybe a
find [] _ = Nothing
find (a : as) f = if f a then Just a else find as f