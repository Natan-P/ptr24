module Naloge.Naloga2 (simple) where

cook1 :: Integer
cook1 = 5
cook2 :: Integer
cook2 = 9

n :: Integer
n = 10000

simple :: String -> String
simple = const . show . length $ filter (\c1 -> (n-(c1*cook1)) `mod` cook2 == 0) [0..(n `div` cook1 + 1)]
