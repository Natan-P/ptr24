module Naloge.Naloga3 (simple) where
import Data.List ( scanl', uncons )
import Data.Maybe (fromJust)
import Data.Function (on)
import Text.Printf (printf)

intDiv :: (Integral i, Fractional n) => i -> i -> n
intDiv = (/) `on` fromIntegral
--intDiv a b = (fromIntegral a) / (fromIntegral b)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x
thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

truncate' :: Int -> Double -> Double
truncate' n x = (fromIntegral (floor (x * t) :: Integer)) / t
    where t = 10^n

simple :: String -> String
simple cont = printf "%0.2g" . truncate' 2 . thd3 . head . dropWhile ((> 0) . fst3) $ scanl' fun (fst startInfo, snd startInfo, 0) ((0, snd startInfo) : trafficInfo)
    where
        lns = lines cont
        mappedLines :: [[Int]]
        mappedLines = map (map (\n -> read n :: Int) . words) lns
        startInfo :: (Int, Int)
        trafficInfo :: [(Int, Int)]
        (startInfo, trafficInfo) = fromJust . uncons $ map (\(a:b:_) -> (a,b)) mappedLines
        fun :: (Int, Int, Double) -> (Int, Int) -> (Int, Int, Double)
        fun (left, speed, time) (dist, lim) = (left - drivenDist, lim, time+(drivenDist `intDiv` speed))
            where delta = dist - (fst startInfo - left)
                  drivenDist :: Int
                  drivenDist = min left delta
