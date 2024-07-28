-- no golfing, we readable this year
module Naloge.Naloga1 (simple) where
import Data.List (scanl')
import Data.Maybe (fromMaybe)

data Rastje = T | M | Z | R
data Zgodovina = Zgodovina { prv :: Rastje
                           , drg :: Rastje
                           , trt :: Rastje
                           , st :: Int
                           , pojed :: Int
                           }

simple :: String -> String
simple cont = show . st . head . dropWhile (\z -> pojed z < sita) . scanl' fun (Zgodovina Z Z Z 0 0) $ lineToRast $ lns !! 2
    where
        fun :: Zgodovina -> Rastje -> Zgodovina
        fun (Zgodovina _ d t i p) Z = Zgodovina d t Z (i+1) p    
        -- ce 5 zaporednih biljk, lahko recemo da je bila 4. zemlja da lahko 5. poje
        fun (Zgodovina T T T i p) _ = Zgodovina T T Z (i+1) p    
        fun (Zgodovina _ d R i p) M = Zgodovina d R M (i+1) p    
        fun (Zgodovina _ d t i p) c = Zgodovina d t c (i+1) (p+1)

        lineToRast = map (fromMaybe Z . flip lookup [('|', T), ('M', M), ('/', Z), ('R', R)])
        
        lns = lines cont
        sita = read (head lns) :: Int
