module Main where

import Data.Bool (bool)
import Data.Foldable (foldMap)
import Data.List (sort, group)
import Data.Monoid (Sum(..))

cr :: String -> (Sum Int, Sum Int)
cr xs =
  let ls = map length . group . sort $ xs
  in (bool 0 1 (2 `elem` ls), bool 0 1 (3 `elem` ls))

match :: String -> String -> Bool
match xs ys = (== 1) . length . filter not $ zipWith (==) xs ys

main :: IO ()
main = do
  input <- lines <$> getContents
  let (Sum x, Sum y) = foldMap cr input
  print $ x * y
  let ((a,b):_) = [(a, b) | a <- input, b <- input, match a b]
  putStrLn $ filter (`elem` b) a
