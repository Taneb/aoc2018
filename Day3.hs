module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
import Text.Read.Lex

parse :: String -> (Int, (Int, Int), (Int, Int))
parse = fst . head . readP_to_S parser
  where
    parser = do
      char '#'
      i <- readDecP
      string " @ "
      x <- readDecP
      char ','
      y <- readDecP
      string ": "
      w <- readDecP
      char 'x'
      h <- readDecP
      eof
      pure (i, (x,y), (w, h))

p1 :: [((Int,Int),(Int,Int))] -> Int
p1 = go S.empty S.empty
  where
    go _ b [] = S.size b
    go a b (((x,y),(w,h)):r) = uncurry go (add a b x y w h) r

    add a b x y w h = fov a b [(x + x',y + y') | x' <- [0..w-1], y' <- [0..h-1]]
    
    fov a b [] = (a,b)
    fov a b (c:r) | c `S.member` a = fov a (S.insert c b) r
                  | otherwise      = fov (S.insert c a) b r

p2 :: [(Int, (Int, Int), (Int, Int))] -> Int
p2 = go M.empty S.empty
  where 
    go _ b [] = S.elemAt 0 b
    go a b ((i, (x,y), (w,h)):r) = uncurry go (add a b i x y w h) r

    add a b i x y w h = case fov a b i [(x + x', y + y') | x' <- [0..w-1], y' <- [0..h-1]] True of
      (a',b',g) | g -> (a', S.insert i b')
                | otherwise -> (a',b')

    fov a b _ [] g = (a,b,g)
    fov a b i (c:r) g = case M.insertLookupWithKey (\_ _ o -> o) c i a of
      (Nothing, a') -> fov a' b i r g
      (Just j, _) -> fov a (S.delete j b) i r False


main :: IO ()
main = do
  input <- map parse . lines <$> getContents
  print . p1 $ map (\(_,c,s) -> (c,s)) input
  print . p2 $ input
