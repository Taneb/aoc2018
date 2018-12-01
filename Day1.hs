module Main where

import qualified Data.IntSet as S

rl :: String -> Int
rl ('+':xs) = read xs
rl ('-':xs) = negate $ read xs
rl _ = error "Incorrect input"

kl :: [Int] -> Int
kl = go (S.singleton 0) 0
 where
  go xs t (i:is) =
    let t' = t + i
    in if t' `S.member` xs
    then t'
    else go (t' `S.insert` xs) t' is

main :: IO ()
main = do
  input <- map rl . lines <$> getContents
  print $ sum input
  print . kl $ cycle input
