-- TODO: document
module Matrix (identity,isIdentity,dyadic,transpose) where

import qualified Data.List as L

transpose :: [[Int]] -> [[Int]]
transpose = L.transpose

identity :: Int -> [[Int]]
identity n
  | n > 1 = L.nub $ L.permutations $ 1 : replicate (n-1) 0
  | otherwise = [[1]]

isIdentity :: [[Int]] -> Bool
isIdentity xs = xs == identity (length xs)

dyadic :: [Int] -> [Int] -> [[Int]]
dyadic xs ys = map (\x -> map (x*) ys) xs
