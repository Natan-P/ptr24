module Naloge.Naloga5 (simple) where
import Data.List (transpose, foldl')

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

simple :: String -> String
simple cont = show . maximum . uncurry (foldl' (addFood . spread)) . (\(x:xs)->(map (\c->if c == 'B' then 0 else -1) x,xs)) $ transpose lns
    where
        lns = lines cont
        depth :: Int
        depth = read $ head lns
        spread :: [Int] -> [Int]
        spread l = map (\i -> maximum $ slice (max 0 (i-1)) (min depth (i+1)) l) [0..depth-1]
        addFood :: [Int] -> String -> [Int]
        addFood = zipWith (\i c -> (if c == 'H' then i + 1 else i))
